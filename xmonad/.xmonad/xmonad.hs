{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main
  ( main
  )
where

import qualified Data.Map                      as M
import           Data.Map                       ( Map )
import           System.Exit                    ( exitSuccess )

-- xmonad core
import           XMonad
import qualified XMonad.StackSet               as W

-- xmonad contrib
import           XMonad.Actions.Minimize        ( maximizeWindowAndFocus
                                                , minimizeWindow
                                                , withLastMinimized
                                                )
import           XMonad.Actions.Navigation2D    ( defaultTiledNavigation
                                                , hybridOf
                                                , lineNavigation
                                                , centerNavigation
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
import           XMonad.Hooks.InsertPosition    ( insertPosition
                                                , Focus(Newer)
                                                , Position(Above, Below)
                                                )
import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                , ToggleStruts(ToggleStruts)
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
                                                ( emptyBSP
                                                , ResizeDirectional
                                                  ( ExpandTowards
                                                  )
                                                , Rotate(Rotate)
                                                , Swap(Swap)
                                                )
import           XMonad.Layout.BoringWindows    ( boringWindows )
import           XMonad.Layout.Minimize         ( minimize )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Spacing          ( Border(Border)
                                                , decScreenWindowSpacing
                                                , incScreenWindowSpacing
                                                , spacingRaw
                                                , toggleScreenSpacingEnabled
                                                , toggleWindowSpacingEnabled
                                                )
import           XMonad.Util.NamedScratchpad    ( NamedScratchpad(NS)
                                                , NamedScratchpads
                                                , namedScratchpadAction
                                                , namedScratchpadFilterOutWorkspacePP
                                                , namedScratchpadManageHook
                                                )
import           XMonad.Util.Types              ( Direction2D(U, D, R, L) )
import           XMonad.Util.SpawnOnce          ( spawnOnce )

-- From lib source directory
import           WalColors                      ( Colors(..)
                                                , SpecialColors(..)
                                                , ColorPalette(..)
                                                , getWalWithFallback
                                                )

-- PROGRAMS --------------------------------------------------------------------

-- | The command to launch a terminal.  `xfce4-terminal' for example.
--
myTerminal :: String
myTerminal = "~/scripts/term.sh"

-- | List of scratchpad windows.  Runs a command to spawn a window if
-- the scratchpad window doesn't exist
--
myScratchpads :: NamedScratchpads
myScratchpads = [NS "terminal" spawnTerminal findTerminal manageTerminal]
 where
  spawnTerminal  = myTerminal ++ " -n \"scratchpad\""
  findTerminal   = appName =? "scratchpad"
  manageTerminal = handleDialog

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
myManageHook =
  insertPosition Below Newer
    <+> composeOne
          [ stringProperty "WM_WINDOW_ROLE" =? "toolbox" -?> handleDialog
          , isDialog -?> handleDialog
          , transience
          ]
    <+> namedScratchpadManageHook myScratchpads

-- LAYOUT ----------------------------------------------------------------------

-- | The available layouts.
--
-- avoidStruts will resize windows to make space for taskbars like
-- polybar.
--
myLayout = layoutMods $ withGaps emptyBSP ||| Full
 where
  layoutMods = avoidStruts . boringWindows . smartBorders . minimize
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
       -- Launch the terminal scratchpad (0x0060 = grave)
       , ( (modm, 0x0060)
         , namedScratchpadAction myScratchpads "terminal"
         )
       -- Cycle through the available layout algorithms.
       , ( (modm, xK_space)
         , sendMessage NextLayout
         )
       --  Reset the layouts on the current workspace to default.
       , ( (modm .|. shiftMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )
       -- Resize viewed windows to the correct size.
       , ( (modm, xK_n)
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
       -- Toggle floating window focus
       , ( (modm, xK_y)
         , switchLayer
         )
       -- Focus window toward the left
       , ( (modm, xK_h)
         , windowGo L False
         )
       -- Focus window toward the right
       , ( (modm, xK_l)
         , windowGo R False
         )
       -- Focus window toward the top
       , ( (modm, xK_k)
         , windowGo U False
         )
       -- Focus window toward the bottom
       , ( (modm, xK_j)
         , windowGo D False
         )
       -- Move focused window toward the left
       , ( (modm .|. shiftMask, xK_h)
         , windowSwap L False
         )
       -- Move focused window toward the right
       , ( (modm .|. shiftMask, xK_l)
         , windowSwap R False
         )
       -- Move focused window toward the top
       , ( (modm .|. shiftMask, xK_k)
         , windowSwap U False
         )
       -- Move focused window toward the bottom
       , ( (modm .|. shiftMask, xK_j)
         , windowSwap D False
         )
       -- Move split towards the left
       , ( (modm .|. controlMask, xK_h)
         , sendMessage $ ExpandTowards L
         )
       -- Move split towards the right
       , ( (modm .|. controlMask, xK_l)
         , sendMessage $ ExpandTowards R
         )
       -- Move split towards the top
       , ( (modm .|. controlMask, xK_k)
         , sendMessage $ ExpandTowards U
         )
       -- Move split towards the bottom
       , ( (modm .|. controlMask, xK_j)
         , sendMessage $ ExpandTowards D
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
       , ((modm, xK_q)              , restart "xmonad" True)
       , ((modm, xK_m)              , withFocused minimizeWindow)
       , ((modm .|. shiftMask, xK_m), withLastMinimized maximizeWindowAndFocus)
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
myPP colorscheme = namedScratchpadFilterOutWorkspacePP $ xmobarPP
  { ppCurrent         = fgColorPad $ foreground $ special colorscheme
  , ppHidden          = fgColorPad $ color8 $ colors colorscheme
  , ppHiddenNoWindows = const ""
  , ppUrgent          = fgColorPad $ color1 $ colors colorscheme
  , ppLayout          = fgColorPad $ foreground $ special colorscheme
  , ppSep             = fgColorPad (color8 $ colors colorscheme) "//"
  , ppTitle           = const ""
  }
  where fgColorPad fg = xmobarColor fg "" . wrap "" "   "

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modm } = (modm, xK_b)

-- RUN XMONAD ------------------------------------------------------------------

myConfig colorscheme =
  withNavigation2DConfig myNavigation2DConfig $ docks $ ewmh def
    { terminal           = myTerminal
    , modMask            = mod4Mask
    , workspaces         = map show [1 .. 9 :: Int]
    , keys               = myKeys
    , borderWidth        = 2
    , normalBorderColor  = background $ special colorscheme
    , focusedBorderColor = color4 $ colors colorscheme
    , handleEventHook    = fullscreenEventHook
    , manageHook         = myManageHook
    , layoutHook         = myLayout
    , startupHook        = spawnOnce "~/scripts/startup.sh --fix-cursor"
    }

myNavigation2DConfig = def { defaultTiledNavigation = hybridNavigation }
  where hybridNavigation = hybridOf lineNavigation centerNavigation

main :: IO ()
main = do
  colorscheme <- getWalWithFallback

  xmonad =<< statusBar "xmobar"
                       (myPP colorscheme)
                       toggleStrutsKey
                       (myConfig colorscheme)
