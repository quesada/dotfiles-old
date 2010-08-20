-- Import statements
import XMonad

import XMonad.Operations

import System.IO
import System.Exit

import Data.List

import Control.Monad (liftM2)

import XMonad.Config.Gnome

import XMonad.Util.Run

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.Spacing
--import XMonad.Layout.Tabbed

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- Define terminal
myTerminal      = "urxvtc"

-- Define window focus color
myFocusedBorderColor = "#77a8bf"

-- Define amount and names of clickable workspaces.
myWorkspaces    = clickable . (map dzenEscape) $ nWorkspaces 0 ["main","web","chat","media","graph","browse","dev","mail"]
    where nWorkspaces n [] = map show [1 .. n]
          nWorkspaces n l  = init l ++ map show [length l .. n] ++ [last l]
          clickable l      = [ "^ca(1,xdotool key ctrl+F" ++ show (n) ++ ")" ++ ws ++ "^ca()" | (i,ws) <- zip [1..] l, let n = i ]

-- Workspace variables
mainWs      = (myWorkspaces !! 0)
webWs       = (myWorkspaces !! 1)
chatWs      = (myWorkspaces !! 2)
mediaWs     = (myWorkspaces !! 3)
graphWs     = (myWorkspaces !! 4)
browseWs    = (myWorkspaces !! 5)
devWs       = (myWorkspaces !! 6)
mailWs      = (myWorkspaces !! 7)

-- default tiling algorithm partitions the screen into two panes
tiled  x  = spacing 10 $ Tall nmaster delta ratio
    where
        -- The default number of windows in the master pane
        nmaster = x

        -- Default proportion of screen occupied by master pane
        ratio   = 1/2

        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

-- Define default layouts used on most workspaces
defaultLayouts  = (tiled 1) ||| Mirror (tiled 1) ||| fullLayout

-- Define layout for specific workspaces
fullLayout     = noBorders $ Full
graphLayout    = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full
imLayout       = reflectHoriz $ withIM (1/3) (Role "main") (Mirror (tiled 2))

-- Put all layouts together
myLayout        = avoidStruts $ onWorkspace webWs fullLayout $ onWorkspace chatWs imLayout $ onWorkspace mediaWs fullLayout $ onWorkspace graphWs graphLayout $ onWorkspace mailWs fullLayout $ defaultLayouts

-- Define keys to add
keysToAdd x     = [ -- Gnome run dialog
                       ((modMask x, xK_F2), spawn "/home/quesada/.run/run.py --interface=full")
                    -- Gnome close window
                    ,  ((modMask x, xK_F4), kill)
                    -- Shift to previous workspace
                    ,  (((modMask x .|. controlMask), xK_Left), prevWS)
                    -- Shift to next workspace
                    ,  (((modMask x .|. controlMask), xK_Right), nextWS)
                    -- Shift window to previous workspace
                    ,  (((modMask x .|. shiftMask), xK_Left), shiftToPrev)
                    -- Shift window to next workspace
                    ,  (((modMask x .|. shiftMask), xK_Right), shiftToNext)
                    -- Grid select applications
                    ,  ((controlMask, xK_space), goToSelected defaultGSConfig)
                    -- Decrease volume
                    ,  ((controlMask, xK_KP_Subtract), spawn "amixer -Dpulse set Master 2%-")
                    -- Increase volume
                    ,  ((controlMask, xK_KP_Add), spawn "amixer -Dpulse set Master 2%+")
                    -- Play/pause mpd
                    ,  ((controlMask, xK_p), spawn "ncmpcpp toggle")
                    -- Play next song in mpd
                    ,  ((controlMask, xK_Right), spawn "ncmpcpp next")
                    -- Play previous song in mpd
                    ,  ((controlMask, xK_Left), spawn "ncmpcpp prev")
                    -- Lock screen with gnome screensaver
                    ,  (((modMask x .|. controlMask), xK_l), spawn "i3lock -d")
                    -- Turn off screen
                    ,  ((controlMask, xK_b), spawn "xset dpms force off")
                    -- Increase master pane count
                    , ((modMask x, xK_KP_Add), sendMessage (IncMasterN 1))
                    -- Decrease master pane count
                    , ((modMask x, xK_KP_Subtract), sendMessage (IncMasterN (-1)))
                    -- Quit xmonad
                    , ((modMask x .|. shiftMask, xK_q), io (exitWith ExitSuccess))
                  ]
                  ++
                  [
                    -- Focus workspace with Ctrl + F1..F8, Shift window to workspace with Ctrl + Shift + F1..F8
                    (  (m .|. controlMask, k), windows $ f i)
                       | (i, k) <- zip (myWorkspaces) [xK_F1 .. xK_F8]
                       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
                  ]


-- Define keys to remove
keysToRemove x  = [
                    -- Old close window binding
                    ,  (modMask x, xK_c)
                    -- Unused gmrun binding
                    ,  (modMask x .|. shiftMask, xK_p)
                    -- Unused gnome session logout dialog
                    ,  (modMask x .|. shiftMask, xK_q)
                    -- Unused increase master pane count
                    ,  (modMask x, xK_comma)
                    -- Unused decrease master pane count
                    ,  (modMask x, xK_period)
                  ]
                  ++
                  [
                       (shiftMask .|. modMask x, k) | k <- [xK_1 .. xK_9]
                  ]
                  ++
                  [
                       ( modMask x, k) | k <- [xK_1 .. xK_9]
                  ]

-- Merge keys to add and existing keys
newKeys x       = M.union (keys gnomeConfig x) (M.fromList (keysToAdd x))

-- Delete the keys to remove from existing keys
myKeys x        = foldr M.delete (newKeys x) (keysToRemove x)

-- Define the workspace an application has to go to
myManageHook    = composeAll . concat $
                  [
                    -- The applications that go to web
                    [ className =? b --> viewShift webWs    | b <- myClassWebShifts]
                    -- The applications that go to chat
                  , [ className =? c --> viewShift chatWs   | c <- myClassChatShifts]
                    -- The applications that go to media
                  , [ className =? d --> viewShift mediaWs  | d <- myClassMediaShifts]
                    -- The applications that go the graph
                  , [ className =? e --> viewShift graphWs  | e <- myClassGraphShifts]
                    -- The applications that go to browse
                  , [ className =? f --> viewShift browseWs | f <- myClassBrowseShifts]
                    -- The applications that go to dev
                  , [ className =? g --> viewShift devWs    | g <- myClassDevShifts]
                  , [ title     =? k --> viewShift devWs    | k <- myTitleDevShifts]
                    -- The applications that go to mail
                  , [ className =? h --> viewShift mailWs   | h <- myClassMailShifts]
                    -- The applications that float
                  , [ className =? i --> doFloat            | i <- myClassFloats]
                  , [ title     =? j --> doFloat            | j <- myTitleFloats]
                  ]
                  where
                        viewShift           = doF . liftM2 (.) W.greedyView W.shift
                        myClassWebShifts    = ["Firefox","Filezilla","chromium"]
                        myClassChatShifts   = ["konversation"]
                        myClassMediaShifts  = ["MPlayer"]
                        myClassGraphShifts  = ["Gimp", "feh"]
                        myClassBrowseShifts = ["Nautilus","File-roller"]
                        myClassDevShifts    = ["Eclipse", "pydev","."]
                        myClassMailShifts   = ["Thunderbird"]
                        myClassFloats       = ["feh","Dialog",".","File-roller"]
                        myTitleFloats       = ["","Run Command"]

myWorkspaceBar = "dzen2 -x '0' -y '0' -h '24' -w '970' -ta 'l' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
myStatusBar = "conky -c .conkyrc_console | dzen2 -x '970' -y '0' -h '24' -w '850' -ta 'r' -fg '#77a8bf' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
conkyMpdBar = "conky -c .conkyrc_mpd | dzen2 -x '0' -y '1176' -h '24' -w '960' -ta 'l' -fg '#77a8bf' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"
conkyTimeBar = "conky -c .conkyrc_date | dzen2 -x '960' -y '1176' -h '24' -w '960' -ta 'r' -fg '#FFFFFF' -bg '#161616' -fn '-*-bitstream vera sans-medium-r-normal-*-11-*-*-*-*-*-*-*'"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ dzenPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#161616" . pad
      , ppVisible           =   dzenColor "white" "#161616" . pad
      , ppHidden            =   dzenColor "white" "#161616" . pad
      , ppHiddenNoWindows   =   dzenColor "#444444" "#161616" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#161616" . dzenStrip
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#77a8bf" "#161616" .
                                (\x -> case x of
                                        "Spacing 10 Tall"         ->      "Tall"
                                        "Spacing 10 Full"         ->      "Full"
                                        "Mirror Spacing 10 Tall"  ->      "Mirror Tall"
                                        "IM ReflectX IM Full"     ->      "IM"
                                        "ReflectX IM Mirror Spacing 10 Tall" ->  "IM"
                                        _                         ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#161616" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

main = do
    workspaceBar <- spawnPipe myWorkspaceBar
    statusBar <- spawnPipe myStatusBar
    conkyMpd <- spawnPipe conkyMpdBar
    conkyTime <- spawnPipe conkyTimeBar
    terminal1 <- spawn myTerminal
    terminal2 <- spawn myTerminal
    terminal3 <- spawn myTerminal
    terminal4 <- spawn myTerminal
    xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $ defaultConfig {
        terminal            = myTerminal
      , focusedBorderColor  = myFocusedBorderColor
      , workspaces          = myWorkspaces
      , keys                = myKeys
      , layoutHook          = myLayout
      , manageHook          = manageDocks <+> myManageHook
      , logHook             = myLogHook workspaceBar  >> setWMName "LG3D"
}
