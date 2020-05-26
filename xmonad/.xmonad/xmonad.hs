{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main
  ( main
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           System.Exit                    ( exitSuccess )

-- xmonad core
import           XMonad
import qualified XMonad.StackSet               as W

-- xmonad contrib
import           XMonad.Actions.Navigation2D    ( Navigation2DConfig
                                                , centerNavigation
                                                , defaultTiledNavigation
                                                , hybridOf
                                                , lineNavigation
                                                , switchLayer
                                                , windowGo
                                                , windowSwap
                                                , withNavigation2DConfig
                                                )
import           XMonad.Hooks.DynamicLog        ( PP(..)
                                                , statusBar
                                                , wrap
                                                , xmobarColor
                                                , xmobarPP
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.InsertPosition    ( Focus(Newer)
                                                , Position(Above, Below)
                                                , insertPosition
                                                )
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(ToggleStruts)
                                                , avoidStruts
                                                , docks
                                                )
import           XMonad.Hooks.ManageHelpers     ( composeOne
                                                , doCenterFloat
                                                , isDialog
                                                , transience
                                                , (-?>)
                                                )
import           XMonad.Hooks.Place             ( fixed
                                                , placeFocused
                                                )
import           XMonad.Layout.BinarySpacePartition
                                                ( ResizeDirectional
                                                  ( ExpandTowards
                                                  )
                                                , Rotate(Rotate)
                                                , Swap(Swap)
                                                , emptyBSP
                                                )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Spacing          ( Border(Border)
                                                , decScreenWindowSpacing
                                                , incScreenWindowSpacing
                                                , spacingRaw
                                                , toggleScreenSpacingEnabled
                                                , toggleWindowSpacingEnabled
                                                )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Util.Types              ( Direction2D(D, L, R, U) )

-- lib
import           WalColors                      ( ColorPalette(..)
                                                , Colors(..)
                                                , SpecialColors(..)
                                                , getWalWithFallback
                                                , oneDarkFallbackColors
                                                )

-- PROGRAMS --------------------------------------------------------------------

-- | The command to launch a terminal.  `xfce4-terminal' for example.
--
myTerminal :: String
myTerminal = "~/scripts/term.sh"

-- MANAGE HOOK -----------------------------------------------------------------

-- | Manage dialog windows.  New windows are automatically focused,
-- and are placed above older windows.  They are positioned on the
-- center of the screen.
--
handleDialog :: ManageHook
handleDialog = insertPosition Above Newer <+> doCenterFloat

-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
--
-- `insertPosition Below Newer' will spawn new windows as a slave
-- window rather than the master window.
--
myManageHook :: ManageHook
myManageHook = insertPosition Below Newer
  <+> composeOne [isDialog -?> handleDialog, transience]

-- LAYOUT ----------------------------------------------------------------------

-- | The available layouts.
--
-- avoidStruts will resize windows to make space for taskbars like
-- polybar.
--
myLayout = layoutMods $ withGaps emptyBSP ||| Full
 where
  layoutMods = avoidStruts . smartBorders
  withGaps   = spacingRaw smartGaps spacing screenGaps spacing windowGaps
   where
    spacing    = Border 10 10 10 10
    smartGaps  = False
    screenGaps = False
    windowGaps = False

-- KEYS ------------------------------------------------------------------------

-- | The xmonad key bindings. Add, modify or remove key bindings here.
--
-- Media keys, application shortcuts, etc defined in sxhkd.
--
myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [
       -- Close focused window.
         ( (modm .|. shiftMask, xK_q)
         , kill
         )
       -- Cycle through the available layout algorithms.
       , ( (modm, xK_space)
         , sendMessage NextLayout
         )
       --  Reset the layouts on the current workspace to default.
       , ( (modm .|. shiftMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )
       -- Move focus to the next window.
       , ( (modm, xK_n)
         , windows W.focusDown
         )
       -- Move focus to the previous window.
       , ( (modm, xK_p)
         , windows W.focusUp
         )
       -- Toggle floating window focus
       , ( (modm, xK_y)
         , switchLayer
         )
       -- Rotate a split (horizontal/vertical)
       , ( (modm, xK_v)
         , sendMessage Rotate
         )
       -- Swap left and right children of a split
       , ( (modm, xK_s)
         , sendMessage Swap
         )
       -- Toggle gaps
       , ( (modm, xK_g)
         , toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled
         )
       -- Increase gap size
       , ( (modm .|. shiftMask, xK_g)
         , incScreenWindowSpacing 2
         )
       -- Decrease gap size
       , ( (modm .|. controlMask, xK_g)
         , decScreenWindowSpacing 2
         )
       -- Move window to the center of the screen
       , ( (modm, xK_c)
         , placeFocused $ fixed (0.5, 0.5)
         )
       -- Push window back into tiling.
       , ( (modm, xK_t)
         , withFocused $ windows . W.sink
         )
       -- Toggle overlap with bar
       , ( toggleStrutsKey conf
         , sendMessage ToggleStruts
         )
       -- Quit xmonad.
       , ( (modm .|. shiftMask .|. controlMask, xK_q)
         , io exitSuccess
         )
       -- Restart xmonad.
       , ( (modm, xK_q)
         , restart "xmonad" True
         )
       ]
    ++
       -- mod-{h,j,k,l}, focus window towards left, down, up, or right
       -- mod-shift-{h,j,k,l}, move window
       -- mod-control-{h,j,k,l}, resize window
       [ ((m .|. modm, key), f dir)
       | (dir, key) <- [(L, xK_h), (D, xK_j), (U, xK_k), (R, xK_l)]
       , (f, m) <-
         [ (flip windowGo False        , 0)
         , (flip windowSwap False      , shiftMask)
         , (sendMessage . ExpandTowards, controlMask)
         ]
       ]
    ++

       -- mod-[1..9], Switch to workspace N
       -- mod-shift-[1..9], Move client to workspace N
       [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++

       -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
       -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
       [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       ]

-- XMOBAR ----------------------------------------------------------------------

myPP :: Colors -> PP
myPP theme = xmobarPP
  { ppCurrent         = xmobarColor (foreground $ special theme) "" . pad
  , ppVisible         = xmobarColor (color4 $ colors theme) "" . pad
  , ppHidden          = xmobarColor (color8 $ colors theme) "" . pad
  , ppHiddenNoWindows = const ""
  , ppUrgent          = xmobarColor (color1 $ colors theme) "" . pad
  , ppLayout          = xmobarColor (foreground $ special theme) "" . pad
  , ppSep             = xmobarColor (color8 $ colors theme) "" $ pad "//"
  , ppTitle           = const ""
  }
  where pad = wrap "" "    "


toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modm } = (modm, xK_b)

-- RUN XMONAD ------------------------------------------------------------------

myConfig theme = def
  { terminal           = myTerminal
  , modMask            = mod4Mask
  , workspaces         = map show [1 .. 9 :: Int]
  , keys               = myKeys
  , borderWidth        = 2
  , normalBorderColor  = color0 $ colors theme
  , focusedBorderColor = color4 $ colors theme
  , handleEventHook    = fullscreenEventHook
  , manageHook         = myManageHook
  , layoutHook         = myLayout
  , startupHook        = spawnOnce "~/scripts/startup.sh --fix-cursor"
  }

myNavigation2DConfig :: Navigation2DConfig
myNavigation2DConfig = def { defaultTiledNavigation = hybridNavigation }
  where hybridNavigation = hybridOf lineNavigation centerNavigation

main :: IO ()
main = do
  theme <- getWalWithFallback oneDarkFallbackColors
  xmonad =<< statusBar
    "xmobar"
    (myPP theme)
    toggleStrutsKey
    (withNavigation2DConfig myNavigation2DConfig $ docks $ ewmh $ myConfig theme
    )
