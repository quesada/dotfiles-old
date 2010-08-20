-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/lars%27_xmonad.hs
import XMonad
import XMonad.Actions.FloatKeys
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import System.Exit

import qualified System.IO.UTF8
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal      = "sakura" 
myBorderWidth   = 1
myModMask       = mod4Mask
myNumlockMask   = mod2Mask
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myKeys = \c -> mkKeymap c $
    -- volume key binding
    [ ("M-=", spawn "aumix2pipe +10")
    , ("M--", spawn "aumix2pipe -10")

    -- moc, music player
    , ("M-p p", spawn "mocp --toggle-pause")
    , ("M-p n", spawn "mocp --next")
    , ("M-p S-n", spawn "mocp --previous")

    -- launch programs
    , ("M-r b b s",   spawn "pcmanx")
    , ("M-r c k",     spawn "conkeror")
    , ("M-r e",       spawn "run-client 'emacsclient -c' 'emacs --daemon'")
    , ("M-r f f",     spawn "firefox")
    , ("M-r h a l t", spawn "sudo shutdown -h now")
    , ("M-r p",       spawn "pidgin")
    , ("M-r s d",     spawn "stardict")
    , ("M-r s s",     spawn "scrot")
    , ("M-r s S-s",   spawn "scrot -s")
    , ("M-r s t",     spawn "killall stalonetray; stalonetray &")
    , ("M-r r",       spawn "gmrun")
    , ("M-r t",       spawn "sakura")
    , ("M-r v",       spawn "gvim")

    -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-S-<Space>", sendMessage FirstLayout)

    -- close focused window 
    , ("M-w", kill)
    -- Resize viewed windows to the correct size
    , ("M-S-r", refresh)

    -- Move window focus
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp  )
    , ("M-<Return>", windows W.focusMaster  )
    -- Swap window
    , ("M-S-j", windows W.swapDown >> windows W.focusDown)
    , ("M-S-k", windows W.swapUp >> windows W.focusUp)
    , ("M-S-<Return>", windows W.swapMaster)

    -- Resize the master area
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)

    -- toggle float/sink
    , ("M-t", withFocused $ windows . W.sink)

    -- Change the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    , ("M-g", sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-q", restart "xmonad" True)
    ] ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [(m ++ (show k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces c) [1 .. 9]
        , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]
    ] ++
    -- moving floating window with key
    [(c ++ m ++ k, withFocused $ f (d x))
         | (d, k) <- zip [\a->(a, 0), \a->(0, a), \a->(0-a, 0), \a->(0, 0-a)] ["<Right>", "<Down>", "<Left>", "<Up>"]
         , (f, m) <- zip [keysMoveWindow, \d -> keysResizeWindow d (0, 0)] ["M-", "M-S-"]
         , (c, x) <- zip ["", "C-"] [20, 2]
    ]
    
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
myLayout = avoidStruts (smartBorders Full ||| smartBorders tiled ||| smartBorders (Mirror tiled))
    where
      tiled   = ResizableTall 1 (1/100) (1/2) []

------------------------------------------------------------------------
-- Window rules:
myManageHook = composeAll
    [ className =? "MPlayer"         --> doFloat
    , className =? "Gimp"            --> doFloat
    , className =? "Stardict"        --> doFloat
    , className =? "Smplayer"        --> doFloat
    , className =? "Wicd-client.py"  --> doFloat
    , className =? "Gxmessage"       --> doFloat
    , className =? "Pidgin"          --> doShift "4"
    , className =? "Conkeror"        --> doShift "2"
    , className =? "Emacs"           --> doShift "3"
    , className =? "Shiretoko"       --> doShift "2"
    , className =? "Gvim"            --> doShift "3"
    , resource  =? "desktop_window"  --> doIgnore
    , className =? "stalonetray"     --> doIgnore ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Status bars and logging
-- >myLogHook = dynamicLogDzen
-- statusBarCmd= "xmobar /home/lars/.xmonad/xmobarrc"
statusBarCmd= "xmobar /home/lars/.xmobarrc"
-- dynamiclog pretty printer
myPP h = defaultPP 
         {
           ppCurrent  = xmobarColor "red" ""
         , ppHidden   = xmobarColor "#ffcc00" ""
--         , ppUrgent   = wrap "<fc=#0099ff>" "</fc>" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
         , ppSep      = " | "
         , ppTitle    = xmobarColor "#aabbcc" ""
         , ppLayout   = \s -> xmobarColor "green" "" $
                        case s of
                          "ResizableTall"        -> "Tall"
                          "Mirror ResizableTall" -> "Wide"
                          "Full"                 -> "Full"
                          _                      -> s

         , ppOutput   = System.IO.UTF8.hPutStrLn h
         }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

main =  do 
  din <- spawnPipe statusBarCmd
  xmonad $ defaultConfig {
      -- simple stuff
      terminal           = myTerminal,
      focusFollowsMouse  = myFocusFollowsMouse,
      borderWidth        = myBorderWidth,
      modMask            = myModMask,
      numlockMask        = myNumlockMask,
      workspaces         = myWorkspaces,
      normalBorderColor  = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,

      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,

      -- hooks, layouts
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = dynamicLogWithPP $ myPP din,
      startupHook        = myStartupHook
    }
