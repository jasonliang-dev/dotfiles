-- -*- eval: (flycheck-mode -1) -*-
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception              ( catch
                                                , SomeException
                                                )
import           Data.Aeson                     ( decode
                                                , FromJSON
                                                )
import qualified Data.ByteString.Lazy          as BS
import           Data.ByteString.Lazy           ( ByteString )
import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics                   ( Generic )
import           System.Directory               ( getHomeDirectory )
import           System.Exit                    ( ExitCode(ExitSuccess)
                                                , exitWith
                                                )

-- xmonad core
import           XMonad
import qualified XMonad.StackSet               as W

-- xmonad contrib
import           XMonad.Actions.Navigation2D    ( Direction2D(U, D, R, L)
                                                , windowGo
                                                , windowSwap
                                                , withNavigation2DConfig
                                                )
import           XMonad.Hooks.DynamicLog        ( PP
                                                , ppCurrent
                                                , statusBar
                                                , wrap
                                                , xmobarColor
                                                , xmobarPP
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageDocks       ( docks
                                                , avoidStruts
                                                )
import           XMonad.Hooks.Place             ( placeFocused
                                                , fixed
                                                )
import           XMonad.Layout.NoBorders        ( noBorders
                                                , smartBorders
                                                )
import           XMonad.Layout.ResizableTile    ( MirrorResize
                                                  ( MirrorShrink
                                                  , MirrorExpand
                                                  )
                                                , ResizableTall(ResizableTall)
                                                )
import           XMonad.Layout.ToggleLayouts    ( ToggleLayout(Toggle)
                                                , toggleLayouts
                                                )
import           XMonad.Util.SpawnOnce          ( spawnOnce )

-- COLOURS ---------------------------------------------------------------------

-- | Types that mirror wal's generated colors.json file
--
data Colors = Colors
  { special :: SpecialColors
  , colors  :: ColorPalette
  } deriving (Show, Generic)

instance FromJSON Colors

data SpecialColors = SpecialColors
  { background :: String
  , foreground :: String
  , cursor     :: String
  } deriving (Show, Generic)

instance FromJSON SpecialColors

data ColorPalette = ColorPalette
  { color0  :: String
  , color1  :: String
  , color2  :: String
  , color3  :: String
  , color4  :: String
  , color5  :: String
  , color6  :: String
  , color7  :: String
  , color8  :: String
  , color9  :: String
  , color10 :: String
  , color11 :: String
  , color12 :: String
  , color13 :: String
  , color14 :: String
  , color15 :: String
  } deriving (Show, Generic)

instance FromJSON ColorPalette

-- | If colors.json generated by wal cannot be read, use this color
-- scheme as a fallback
--
fallBackColors :: Colors
fallBackColors = Colors
  { special = SpecialColors { background = "#282c34"
                            , foreground = "#abb2bf"
                            , cursor     = "#abb2bf"
                            }
  , colors  = ColorPalette { color0  = "#282c34"
                           , color1  = "#e06c75"
                           , color2  = "#98c379"
                           , color3  = "#e5c07b"
                           , color4  = "#61afef"
                           , color5  = "#c678dd"
                           , color6  = "#56b6c2"
                           , color7  = "#abb2bf"
                           , color8  = "#545862"
                           , color9  = "#e06c75"
                           , color10 = "#98c379"
                           , color11 = "#e5c07b"
                           , color12 = "#61afef"
                           , color13 = "#c678dd"
                           , color14 = "#56b6c2"
                           , color15 = "#c8ccd4"
                           }
  }

-- LAYOUT ----------------------------------------------------------------------

-- | The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ smartBorders $ toggleLayouts
  (noBorders Full)
  (tiled ||| Mirror tiled)
 where
  -- Unlike Tall, ResizableTall can resize windows vertically
  tiled   = ResizableTall nmaster delta ratio []
  -- The default number of windows in the master pane
  nmaster = 1
  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2
  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

-- KEYS ------------------------------------------------------------------------

-- | The xmonad key bindings. Add, modify or remove key bindings here.
--
-- Media keys, application shortcuts, etc defined in sxhkd.
--
myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig { XMonad.modMask = modMask }) =
  M.fromList
    $
       -- Close focused window.
       [ ( (modMask .|. shiftMask, xK_q)
         , kill
         )

       -- Launch dmenu.
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

       -- Toggle between current layout and fullscreen layout
       , ( (modMask, xK_f)
         , sendMessage (Toggle "Full")
         )

       -- Resize viewed windows to the correct size.
       , ( (modMask, xK_n)
         , refresh
         )

       -- Move focus to the next window.
       , ( (mod1Mask, xK_Tab)
         , windows W.focusDown
         )

       -- Move focus to the previous window.
       , ( (mod1Mask .|. shiftMask, xK_Tab)
         , windows W.focusUp
         )

       -- Swap the focused window and the master window.
       , ( (modMask, xK_m)
         , windows W.swapMaster
         )

       -- Focus window toward the left
       , ( (modMask, xK_h)
         , windowGo L False
         )

       -- Focus window toward the right
       , ( (modMask, xK_l)
         , windowGo R False
         )

       -- Focus window toward the top
       , ( (modMask, xK_k)
         , windowGo U False
         )

       -- Focus window toward the bottom
       , ( (modMask, xK_j)
         , windowGo D False
         )

       -- Focus window toward the left
       , ( (modMask .|. shiftMask, xK_h)
         , windowSwap L False
         )

       -- Focus window toward the right
       , ( (modMask .|. shiftMask, xK_l)
         , windowSwap R False
         )

       -- Focus window toward the top
       , ( (modMask .|. shiftMask, xK_k)
         , windowSwap U False
         )

       -- Focus window toward the bottom
       , ( (modMask .|. shiftMask, xK_j)
         , windowSwap D False
         )

       -- Shrink master horizontally. Resize left.
       , ( (modMask .|. controlMask, xK_h)
         , sendMessage Shrink
         )

       -- Expand master horizontally. Resize right.
       , ( (modMask .|. controlMask, xK_l)
         , sendMessage Expand
         )

       -- Shrink master vertically. Resize down.
       , ( (modMask .|. controlMask, xK_j)
         , sendMessage MirrorShrink
         )

       -- Expand master vertically. Resize up.
       , ( (modMask .|. controlMask, xK_k)
         , sendMessage MirrorExpand
         )

       -- Move window to the center of the screen
       , ( (modMask, xK_c)
         , placeFocused $ fixed (0.5, 0.5)
         )

       -- Push window back into tiling.
       , ( (modMask, xK_t)
         , withFocused $ windows . W.sink
         )

       -- Increment the number of windows in the master area.
       , ( (modMask, xK_equal)
         , sendMessage (IncMasterN 1)
         )

       -- Decrement the number of windows in the master area.
       , ( (modMask, xK_minus)
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

-- XMOBAR ----------------------------------------------------------------------

myPP :: PP
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

-- RUN XMONAD ------------------------------------------------------------------

-- | A wrapper for ByteString.readFile.  It returns empty string where
-- there would usually be an exception.
--
safeReadFile :: FilePath -> IO ByteString
safeReadFile file = BS.readFile file `catch` \(_ :: SomeException) -> return ""

main :: IO ()
main = do
  home <- getHomeDirectory
  json <- safeReadFile $ home ++ "/.cache/wal/colors.json"
  let colorscheme = fromMaybe fallBackColors (decode json :: Maybe Colors)

  xmonad =<< statusBar
    "xmobar"
    myPP
    toggleStrutsKey
    (withNavigation2DConfig def $ ewmh $ myConfig colorscheme)
 where
  myConfig colorscheme = def
    { terminal           = "~/scripts/term.sh"
    , modMask            = mod4Mask
    , keys               = myKeys
    , borderWidth        = 2
    , normalBorderColor  = background $ special colorscheme
    , focusedBorderColor = color6 $ colors colorscheme
    , handleEventHook    = fullscreenEventHook
    , layoutHook         = myLayout
    , startupHook        = spawnOnce $ "~/scripts/startup.sh --fix-cursor"
    }
