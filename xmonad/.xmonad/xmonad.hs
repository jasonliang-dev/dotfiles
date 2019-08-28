-- -*- eval: (flycheck-mode -1) -*-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Control.Arrow                  ( Arrow
                                                , (***)
                                                )
import           Control.Exception              ( catch
                                                , SomeException
                                                )
import           GHC.Generics                   ( Generic )
import           System.Exit                    ( exitWith
                                                , ExitCode(..)
                                                )
import           System.Directory               ( getHomeDirectory )
import           Data.Aeson                     ( decode
                                                , FromJSON
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import qualified Data.ByteString.Lazy          as BS
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.Map                      as M
import           Data.Map                       ( Map )

-- xmonad core
import           XMonad
import qualified XMonad.StackSet               as W

-- xmonad contrib
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Layout.NoBorders        ( noBorders
                                                , smartBorders
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )

-- | Colors type derived from pywal generated json
--
data Colors = Colors
  { special :: SpecialColors
  , colors  :: ColorPalette
  } deriving (Show, Generic)

data SpecialColors = SpecialColors
  { background :: !Text
  , foreground :: !Text
  , cursor     :: !Text
  } deriving (Show, Generic)

data ColorPalette = ColorPalette
  { color0  :: !Text
  , color1  :: !Text
  , color2  :: !Text
  , color3  :: !Text
  , color4  :: !Text
  , color5  :: !Text
  , color6  :: !Text
  , color7  :: !Text
  , color8  :: !Text
  , color9  :: !Text
  , color10 :: !Text
  , color11 :: !Text
  , color12 :: !Text
  , color13 :: !Text
  , color14 :: !Text
  , color15 :: !Text
  } deriving (Show, Generic)

instance FromJSON Colors
instance FromJSON SpecialColors
instance FromJSON ColorPalette

-- | The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| noBorders Full
 where
  -- default tiling algorithm partitions the screen into two panes
  tiled   = Tall nmaster delta ratio
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2
  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

-- | The xmonad key bindings. Add, modify or remove key bindings here.
--
-- (The comment formatting character is used when generating the manpage)
--
myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $
  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
       [ ((modMask .|. shiftMask, xK_q), kill)
       , ( (modMask, xK_p)
         , spawn "dmenu_run"
         )

  -- Cycle through the available layout algorithms.
       , ( (modMask, xK_space)
         , sendMessage NextLayout
         )

  --  Reset the layouts on the current workspace to default.
       , ( (modMask .|. shiftMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )

  -- Resize viewed windows to the correct size.
       , ( (modMask, xK_n)
         , refresh
         )

  -- Move focus to the next window.
       , ( (modMask, xK_Tab)
         , windows W.focusDown
         )

  -- Move focus to the next window.
       , ( (modMask, xK_j)
         , windows W.focusDown
         )

  -- Move focus to the previous window.
       , ( (modMask, xK_k)
         , windows W.focusUp
         )

  -- Swap the focused window and the master window.
       , ( (modMask, xK_m)
         , windows W.swapMaster
         )

  -- Swap the focused window with the next window.
       , ( (modMask .|. shiftMask, xK_j)
         , windows W.swapDown
         )

  -- Swap the focused window with the previous window.
       , ( (modMask .|. shiftMask, xK_k)
         , windows W.swapUp
         )

  -- Shrink the master area.
       , ( (modMask, xK_h)
         , sendMessage Shrink
         )

  -- Expand the master area.
       , ( (modMask, xK_l)
         , sendMessage Expand
         )

  -- Push window back into tiling.
       , ( (modMask, xK_t)
         , withFocused $ windows . W.sink
         )

  -- Increment the number of windows in the master area.
       , ( (modMask, xK_comma)
         , sendMessage (IncMasterN 1)
         )

  -- Decrement the number of windows in the master area.
       , ( (modMask, xK_period)
         , sendMessage (IncMasterN (-1))
         )

  -- Quit xmonad.
       , ( (modMask .|. shiftMask .|. controlMask, xK_q)
         , io (exitWith ExitSuccess)
         )

  -- Restart xmonad.
       , ((modMask, xK_q), restart "xmonad" True)
       ]
    ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
       [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
       [ ( (m .|. modMask, key)
         , screenWorkspace sc >>= flip whenJust (windows . f)
         )
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       ]

-- | Tuple map utility function
--
mapTuple :: Arrow a => a b c -> a (b, b) (c, c)
mapTuple f = f *** f

-- | A wrapper for ByteString.readFile.  It returns empty string where
-- there would usually be an exception.
--
safeReadFile :: FilePath -> IO ByteString
safeReadFile file = BS.readFile file `catch` returnEmpty
 where
  returnEmpty :: SomeException -> IO ByteString
  returnEmpty _ = return ""

-- | Get a pair of colors.  Trys to fetch colors generated by wal.
-- Returns ("#222222", "#808080") if cannot get wal colors.
--
getColors :: IO (Text, Text)
getColors = do
  home <- getHomeDirectory
  json <- safeReadFile $ home ++ "/.cache/wal/colors.json"
  return $ case decode json :: Maybe Colors of
    Nothing  -> ("#222222", "#808080")
    Just col -> (background $ special col, foreground $ special col)

main :: IO ()
main = do
  (normalColor, focusedColor) <- mapTuple unpack <$> getColors
  xmonad $ ewmh def
    { terminal           = "~/scripts/term.sh"
    , modMask            = mod4Mask
    , keys               = myKeys
    , borderWidth        = 2
    , normalBorderColor  = normalColor
    , focusedBorderColor = focusedColor
    , handleEventHook    = fullscreenEventHook
    , layoutHook         = smartBorders myLayout
    , startupHook        = spawnOnce "~/scripts/startup.sh --fix-cursor"
    }
