-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/lorincs_xmonad.hs
-- xmonad-0.9 config using xcompmgr and kde4
import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import Data.Ratio
import XMonad.Layout.LayoutHints
import XMonad.Hooks.FadeInactive
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map        as M
import XMonad.Hooks.Place


--Fading
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
   where fadeAmount = 0.45

-- Workspaces

myWorkspaces = map show [1 .. 6 :: Int]

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modMask, button5), (\_ -> moveTo Next NonEmptyWS))
    , ((modMask, button4), (\_ -> moveTo Prev NonEmptyWS ))

    , ((modMask .|. shiftMask, button5), (\w -> focus w >> kill ))
    ]

-- Layout options:
myLayout = avoidStruts $ onWorkspace "1" (resizableTile ||| Mirror resizableTile) $ smartBorders (resizableTile ||| Mirror resizableTile ||| Full)
     where
        resizableTile = ResizableTall nmaster delta ratio []
        nmaster = 1
        ratio = toRational (2/(1+sqrt(5)::Double))
        delta = 1/100


main = xmonad $ ewmh kde4Config

 { workspaces = myWorkspaces
 , modMask = mod4Mask -- use the Windows button as mod
 , terminal = "urxvt"
 , normalBorderColor  = "#333333"
 , focusedBorderColor = "#666699"
 , layoutHook = myLayout
 , manageHook = manageHook kde4Config <+> myManageHook
 , mouseBindings      = myMouseBindings
 , logHook = myLogHook
 }
 where
   myManageHook = composeAll . concat $
     [ [ className   =? c --> doFloat           | c <- myFloats]
     , [ title       =? t --> doFloat           | t <- myOtherFloats]
     , [ title       =? c --> doF (W.shift "1") | c <- termApps]
     , [ className   =? c --> doF (W.shift "2") | c <- webApps]
     , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
     , [ title       =? c --> doF (W.shift "6") | c <- schedApps]
     , [ className   =? c --> doF (W.shift "6") | c <- schederApps]
     ]
   myFloats      = ["MPlayer", "Gimp", "Smplayer", "Kget", "kget"]
   myOtherFloats = ["alsamixer"]
   termApps      = ["ang@localhost", "root@localhost", "root@binary"]    -- open on desktop 1
   webApps       = ["Firefox", "Opera", "Akregator", "chrome", "Chrome"] -- open on desktop 2
   ircApps       = ["Ksirc", "krusader", "Krusader", "xchat"]            -- open on desktop 3
   schedApps     = ["scheduler"]                -- open on desktop 7
   schederApps   = ["kget"]                     -- open on desktop 7
