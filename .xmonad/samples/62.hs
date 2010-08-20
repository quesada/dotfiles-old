-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/nomeata%E2%80%99s_xmonad.hs

import XMonad
import System.Exit

import Data.Ratio ((%))

import XMonad.Hooks.EventHook
import System.IO
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Actions.UpdatePointer
-- import XMonad.Layout.ShowWName
-- import XMonad.Actions.FlexibleManipulate hiding (position)
import XMonad.Prompt
import XMonad.Prompt.Shell


import qualified XMonad.StackSet as W
import qualified Data.Map        as M


shellPromptConfig = defaultXPConfig
	{ position = Top
--	, showCompletionOnTab = True
	}

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask              , xK_e),      spawn $ XMonad.terminal conf)

    , ((modMask              , xK_s     ), spawn "sm")

    --, ((modMask              , xK_r     ), spawn "gmrun")
    , ((modMask              , xK_r     ), shellPrompt shellPromptConfig)

    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modMask              , xK_f), sendMessage ToggleLayout)

    -- Resize viewed windows to the correct size
    --, ((modMask .|. shiftMask, xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask .|. shiftMask, xK_k     ), sendMessage MirrorExpand)

    -- Shrink the master area
    , ((modMask .|. shiftMask, xK_j     ), sendMessage MirrorShrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask .|. shiftMask, xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> 
	  trace "Restarting xmonad" >>
	  restart "xmonad" True
	  )
    , ((modMask,               xK_Down), moveTo Next HiddenWS)
    , ((modMask,               xK_Up),   moveTo Prev HiddenWS)
    , ((modMask .|. shiftMask, xK_Down), shiftTo Next HiddenWS >> moveTo Next HiddenWS)
    , ((modMask .|. shiftMask, xK_Up),   shiftTo Prev HiddenWS >> moveTo Prev HiddenWS)
    , ((modMask,               xK_w), nextScreen) 
    , ((modMask,               xK_d), swapNextScreen)
    , ((modMask .|. shiftMask, xK_d), shiftNextScreen)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    --[((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_a] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w ))
    --[ ((modMask, button1), (\w -> focus w >> mouseWindow discrete w ))

    -- mod-button2, Set the window to floating mode and resize by dragging
    , ((modMask, button2), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

data EventHookExample = EventHookExample deriving ( Show, Read )
instance EventHook EventHookExample where
       handleEvent _ e = io $ hPutStrLn stderr . show $ e --return ()

--myLayout = {- eventHook EventHookExample $-}  ewmhDesktopsLayout $ avoidStruts $
--		smartBorders $ toggleLayouts Full (tiled ||| Mirror tiled)
myLayout = -- eventHook EventHookExample $
           ewmhDesktopsLayout $ 
	   --smartBorders $
--	   showWName $
	   avoidStruts $
	   toggleLayouts Full $
 	   reflectHoriz $
	   withIM (1%6) (And (ClassName "Pidgin") (Role "buddy_list")) $
 	   reflectHoriz $
	   tiled ||| Mirror tiled
  where
     --tiled   = Tall nmaster delta ratio
     tiled   = ResizableTall 1 (3/100) (1/2) []

myManageHook = composeAll
    [ manageDocks
    , className =? "MPlayer"        --> doFloat
    ]

myLogHook = ewmhDesktopsLogHook
	    --updatePointer Nearest
	    >> updatePointer (Relative 0.9 0.9)

main = xmonad $ defaultConfig {
      -- simple stuff
        terminal           = "gnome-terminal",
        borderWidth        = 0,
        modMask            = mod4Mask,
	workspaces         = ["1","2","3","4","5","6"],
	normalBorderColor  = "#dddddd",
	focusedBorderColor = "#ff0000",

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook
    }
