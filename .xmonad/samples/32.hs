-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/nattfodd%27s_xmonad.hs
import XMonad
import XMonad.Layout
import XMonad.Operations
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.DwmPromote
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Window
import XMonad.Prompt.Shell
import qualified Data.Map as M
import Graphics.X11
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import Data.Ratio ((%))
import System.IO

-- XMonadContrib
-- import XMonad.Util.Dzen
import XMonad.Layout.NoBorders
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.Run

myNormalBGColor     = "#2e3436"
myFocusedBGColor    = "#414141"
myNormalFGColor     = "#babdb6"
myFocusedFGColor    = "#73d216"
myUrgentFGColor     = "#f57900"
myUrgentBGColor     = myNormalBGColor
mySeperatorColor    = "#2e3436"


main = do
    h <- spawnPipe ("dzen2" ++ " " ++ flags)
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { workspaces         = workspaces'
        , layoutHook         = layout'
        , manageHook         = manageDocks <+> manageHook defaultConfig
        , terminal           = "/home/heimdall/scripts/urxvt"
        , normalBorderColor  = "#dddddd"
        , logHook            = dynamicLogWithPP $ myPP h
        , focusedBorderColor = "#aa0000" }
        `additionalKeys` keys'
        where
            fg    = "'grey70'"
            bg    = "'#2c2c32'"
            fn    = "'-*-profont-*-*-*-*-12-*-*-*-*-*-*'"
            flags = "-e '' -w 600 -ta l -fg " ++ fg ++ " -bg " ++ bg ++ " -fn " ++ fn


-- workspaces' = ["α","β","γ","δ","ε","ζ"] ++ map show [7 .. 15 :: Int]
workspaces' = map show [1 .. 13 :: Int]

manageHook' = manageDocks <+> 
            composeAll
            [ className =? "MPlayer" --> doFloat
            , className =? "gecko" --> doF (W.shift "2")]


-- shamelessly stolen from Xilon's config
myPP handle = defaultPP {
        ppCurrent = wrap ("^fg(" ++ myFocusedFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppUrgent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myUrgentBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p(4)") "^p(4)^fg()^bg()",
        ppSep     = "^fg(" ++ mySeperatorColor ++ ")^r(3x3)^fg()",
        ppTitle   = wrap (" ^fg(" ++ myFocusedFGColor ++ ")") "^fg()" ,
        ppOutput  = hPutStrLn handle
}


layout' = avoidStruts (smartBorders (tiled ||| Mirror tiled ||| Full))
          where
              tiled   = Tall nmaster delta ratio
              nmaster = 1     -- The default number of windows in the master pane
              ratio   = 2%3   -- Default proportion of screen occupied by master pane
              delta   = 5%100 -- Percent of screen to increment by when resizing panes

keys' = [((m .|. mod1Mask, k), windows $ f i)
            | (i, k) <- zip workspaces' [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0,xK_a,xK_z,xK_e,xK_r]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        ++ 
        [((mod1Mask               , 0x3b), sendMessage (IncMasterN (-1)))]-- %! Deincrement the number of windows in the master area
        ++
        [((mod1Mask               , xK_b), sendMessage ToggleStruts)] -- Toggle dzen gap
        ++
        [((mod1Mask               , xK_Return), dwmpromote)] -- cycle master window and first slave
        ++
        [((mod1Mask               , xK_m), manPrompt defaultXPConfig)
        ,((mod1Mask               , xK_s), sshPrompt defaultXPConfig)
        ,((mod1Mask .|. shiftMask , xK_p), shellPrompt defaultXPConfig)
        ,((mod1Mask               , xK_u), safePrompt "firefox" defaultXPConfig)
        ,((mod1Mask               , xK_g), windowPromptGoto defaultXPConfig)]



-- Launch xmonad with the following:
-- 
-- #!/bin/sh
-- 
-- FG='grey70' 
-- BG='#2c2c32' 
-- FONT='-*-profont-*-*-*-*-12-*-*-*-*-*-*'
-- 
-- while (true); do ~/scripts/weather.pl; done &
-- xmonad-acpi | dzen2 -e '' -x 600 -w 768 -ta r -fg $FG -bg $BG -fn $FONT &
-- xmonad &
-- 
-- wait $!
-- pkill -HUP dzen2
-- pkill -HUP -f xmonad-acpi
-- wait


