-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/rtalreja%27s_xmonad.hs
--
-- xmonad config file.
--
 
-- XMonad Core
import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 
-- GHC hierarchical libraries
import XMonad.Operations
import XMonad.Config
import XMonad.Util.Run
import System.IO
import Data.Ratio ((%))

--Contribs
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive


-- import XMonad.Layout
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat

 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtcd"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 0
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask	= mod4Mask
altMask 	= mod1Mask 
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
--
myWorkspaces    = ["web","mail","music","im","code","tex"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#000000"
 
------------------------------------------------------------------------
-- Key bindings
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask,                xK_space), spawn $ XMonad.terminal conf)

    -- MPC stuff
    , ((modMask,		xK_n	), spawn "mpc next")
    , ((modMask,		xK_p	), spawn "mpc prev")
    , ((modMask,		xK_semicolon	), spawn "mpc toggle")

    -- launch gmrun
    , ((modMask,		xK_F1	), spawn "gmrun")

    -- launch thunar
    , ((modMask .|. altMask,	xK_a	), spawn "thunar")

    -- launch alsamixer
    , ((modMask .|. altMask,	xK_c	), spawn "firefox -new-window https://mail.google.com/mail/#compose")

    -- launch web-browser
    , ((modMask .|. altMask,	xK_space	), spawn "firefox")
    
    -- launch mutt
    , ((modMask .|. altMask,	xK_m	), spawn "urxvt -e mutt")

    -- launch ncmpcpp
    , ((modMask .|. altMask,	xK_n	), spawn "urxvt -e ncmpcpp")

    -- launch pidgin
    , ((modMask .|. altMask,	xK_p	), spawn "pidgin")

    -- launch rhythmbox
    , ((modMask .|. altMask,	xK_b	), spawn "rhythmbox")

    -- close focused window
    , ((modMask,		xK_x     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,                xK_Return ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    -- , ((modMask .|. shiftMask,  xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    -- , ((modMask,             xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,                xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,                xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,                xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    -- , ((modMask,             xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    --, ((modMask,              xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask,  xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask,  xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,                xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,                xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,                xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask,                xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask,                xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
    , ((modMask,                xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad (Default)
    , ((modMask .|. shiftMask,  xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask,                xK_r     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++
 
    --
    -- mod-[1..6], Switch to workspace N
    -- mod-shift-[1..6], Move client to workspace N
    --

    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_q, xK_w, xK_e ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{u,i}, Switch to {prev,next} workspace
    -- mod-shift-{u,i}, Move client and shift to {prev,next} workspace
    -- Requires Xmonad.Actions.CycleWS
    --
    [	((modMask		, xK_u	), prevWS)
      ,	((modMask		, xK_i	), nextWS)
      ,	((modMask .|. shiftMask	, xK_u	), shiftToPrev >> prevWS)
      ,	((modMask .|. shiftMask	, xK_i	), shiftToNext >> nextWS)
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
-- 
imLayout = smartBorders $ IM (1%5) 
			  (Or (Title "Buddy List") 
			  (And (Resource "main") (ClassName "psi")))

genericLayout =	tiled 
	    |||	Mirror tiled 
	    |||	Full 
	    |||	Grid 
	    |||	Accordion
	    ||| simpleFloat
	    ||| imLayout
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
 

myLayout = 	onWorkspace "4" imLayout 
	    $ 	genericLayout

------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll
    [ className =? "Xpdf"		--> doFloat
    , className =? "gmrun"		--> doFloat
    , className =? "Firefox"		--> doF (W.shift "8")
    , className =? "Pidgin"		--> doF (W.shift "9")
    , resource =? "desktop_window"	--> doIgnore
    , resource =? "kdesktop"		--> doIgnore
    , title =? "Xfce Settings Manager"	--> doF (W.shift "8")
    ]

 
-- Whether focus follows the mouse pointer.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
 
-- Dzen stuff
--
myStatusBar :: String
myStatusBar = "dzen2 -fn '-*-terminus-bold-r-normal-*-12-*-*-*-*-*-*-*' -bg '#000000' -fg '#444444' -h 22 -sa c -x 0 -y 1028 -e '' -ta l"

myConkyBar :: String
myConkyBar = "sleep 1 && conky -c ~/.conkyrcdzen | dzen2 -fn '-*-terminus-bold-r-normal-*-12-*-*-*-*-*-*-*' -bg black -fg green -h 22 -sa c -x 800 -y 1028 -e '' -ta r"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
      {   ppCurrent	= dzenColor "green" "black" . pad
	, ppVisible	= dzenColor "lightgreen" "" . pad
	, ppHidden	= dzenColor "white" "" . pad
	, ppHiddenNoWindows = dzenColor "#444444"  "" . pad
	, ppUrgent	= dzenColor "" "red"
	, ppWsSep    = ""
	, ppSep      = "|"
	, ppLayout   = dzenColor "green" "" .
			  (\ x -> case x of
			      "Maximize Tall" 			-> "[]="
			      "Maximize Mirror Tall"		-> "TTT"
			      "Maximize Full"			-> "<M>"
			      "Maximize Grid"			-> "+++"
			      "Maximize Spiral"			-> "(@)"
			      "Maximize Accordion"		-> "Acc"
			      "Maximize Tabbed Simplest"	-> "Tab"
			      "Maximize Tabbed Bottom Simplest"	-> "TaB"
			      "Maximize SimplestFloat"		-> "><>"
			      "Maximize IM"			-> "IM "
			      "Maximize Dishes 2 (1%6)"		-> "Dsh"
			      _				-> pad x
			  )
	, ppTitle    = (" " ++) . dzenColor "green" "" . dzenEscape
	, ppOutput   = hPutStrLn h

      }

------------------------------------------------------------------------

main :: IO ()
main = do 
       workspaceBarPipe <- spawnPipe myStatusBar 
       conkyBarPipe <- spawnPipe myConkyBar
       spawn "xcompmgr"
       xmonad $ withUrgencyHook NoUrgencyHook defaultConfig {

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
         manageHook         = myManageHook <+> manageDocks,
         logHook	    = myLogHook workspaceBarPipe >> fadeInactiveLogHook 0xdddddddd,

      -- For use with no panels or just dzen2
         layoutHook         = ewmhDesktopsLayout $ avoidStruts $ myLayout
    }

