-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Wraithan%27s_xmonad.hs
import XMonad hiding (Tall)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutHints
import XMonad.Layout.ResizableTile
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run(spawnPipe)

import System.Exit
import System.IO
import Data.Monoid

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook
           $ defaultConfig {
                terminal           = "urxvt",
                modMask            = mod4Mask,
                workspaces         = ["1:Chat", "2:Web", "3:Code", "4:Terms", "5", "6", "7", "8", "9:Music"],
                normalBorderColor  = "#333333",
                focusedBorderColor = "#3399cc",
                manageHook         = myManageHook,
                keys               = myKeys,
                mouseBindings      = myMouseBindings,
                layoutHook         = myLayout,
                logHook            = dynamicLogWithPP $ xmobarPP { 
                                         ppOutput = hPutStrLn xmproc,
                                         ppTitle = xmobarColor "green" "" . shorten 50
                                     }
    }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    [ ((modMask              , xK_Return   ), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c        ), kill)
    , ((modMask              , xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space    ), setLayout $ XMonad.layoutHook conf)
    , ((modMask              , xK_n        ), refresh)
    , ((modMask              , xK_Tab      ), windows W.focusDown)
    , ((modMask              , xK_j        ), windows W.focusDown)
    , ((modMask              , xK_k        ), windows W.focusUp)
    , ((modMask              , xK_m        ), windows W.focusMaster)
    , ((modMask .|. shiftMask, xK_Return   ), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j        ), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k        ), windows W.swapUp)
    , ((modMask              , xK_h        ), sendMessage Shrink)
    , ((modMask              , xK_l        ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h        ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l        ), sendMessage MirrorExpand)
    , ((modMask              , xK_t        ), withFocused $ windows . W.sink)
    , ((modMask              , xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period   ), sendMessage (IncMasterN (-1)))
    , ((modMask .|. shiftMask, xK_q        ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q        ), spawn "xmonad --recompile; xmonad --restart")
    , ((modMask              , xK_F2       ), shellPrompt defaultXPConfig)
    , ((0                    , 0x1008ff13  ), spawn "amixer -q set Master 2dB+")
    , ((0                    , 0x1008ff11  ), spawn "amixer -q set Master 2dB-")
    , ((0                    , 0x1008ff12  ), spawn "amixer -q set Master toggle")
    , ((0                    , 0x1008ff16  ), spawn "mocp --prev")
    , ((0                    , 0x1008ff17  ), spawn "mocp --next")
    , ((0                    , 0x1008ff14  ), spawn "mocp --toggle-pause")
    , ((modMask              , xK_Print    ), spawn "scrot -e 'mv $f ~/Screenshots'")
    ]
    ++

    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++

    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    ]

myLayout = avoidStruts $ layoutHints (tall ||| Mirror tall ||| Full)
  where
     tall = ResizableTall nmaster delta ratio []
     nmaster = 1
     delta   = 3/100
     ratio   = 1/2

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Pidgin"    --> doShift "1:Chat"
    , className =? "Shiretoko" --> doShift "2:Web"]
