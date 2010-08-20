-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/gray_hemp%27s_xmonad.hs
-- dzen2 plus conky config with urgency for xmonad-0.9*
-- uses icons from dzen.geekmode.org
import XMonad
import XMonad.Core

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import qualified Data.Map as M
import Graphics.X11.Xlib
import XMonad.Util.Run

main = do
  myStatusBarPipe <- spawnPipe myStatusBar
  spawn myCPUBar
  spawn myBatteryBar
  spawn myTimeBar
  spawn myXxkbBar
  xmonad $ myUrgencyHook $ defaultConfig {
    terminal = myTerminal,
    normalBorderColor = myInactiveBorderColor,
    focusedBorderColor = myActiveBorderColor,
    workspaces = myWorkspaces,
    modMask = myModMask,
    keys = myKeys,
    manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig,
    layoutHook = avoidStruts $ myLayoutHook,
    logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
    }

-- Fonts
myFont = "xft:monospace:size=8"
mySmallFont = "xft:monospace:size=5"

-- Paths
myBitmapsPath = "/home/gray/.dzen/bitmaps/"

-- Colors
myBgBgColor = "black"
myFgColor = "gray80"
myBgColor = "gray20"

myHighlightedFgColor = "white"
myHighlightedBgColor = "gray40"

myActiveBorderColor = "gray80"
myInactiveBorderColor = "gray20"

myCurrentWsFgColor = "white"
myCurrentWsBgColor = "gray40"
myVisibleWsFgColor = "gray80"
myVisibleWsBgColor = "gray20"
myHiddenWsFgColor = "gray80"
myHiddenEmptyWsFgColor = "gray50"
myUrgentWsBgColor = "brown"
myTitleFgColor = "white"

myUrgencyHintFgColor = "white"
myUrgencyHintBgColor = "brown"

-- Bars
myDzenBarGeneralOptions = "-fn '" ++ myFont ++ "' -fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "'"

myStatusBar = "dzen2 -w 952 -ta l " ++ myDzenBarGeneralOptions
myCPUBar = "conky -c ~/.conky_cpu | sh | dzen2 -x 953 -w 90 -ta l " ++ myDzenBarGeneralOptions
myBatteryBar = "conky -c ~/.conky_battery | sh | dzen2 -x 1044 -w 63 -ta l " ++ myDzenBarGeneralOptions
myTimeBar = "conky -c ~/.conky_time | dzen2 -x 1108 -w 156 -ta c " ++ myDzenBarGeneralOptions
myXxkbBar = "xxkb" -- configuration in ~/.xxkbrc

-- Prefered terminal
myTerminal = "urxvt"

-- Rebind Mod to Windows key
myModMask = mod4Mask

-- Prompt config
myXPConfig = defaultXPConfig {
  position = Bottom,
  promptBorderWidth = 0,
  font = myFont,
  height = 15,
  bgColor = myBgColor,
  fgColor = myFgColor,
  fgHLight = myHighlightedFgColor,
  bgHLight = myHighlightedBgColor
  }

-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)

-- Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  -- Use shellPrompt instead of default dmenu
  ((modm, xK_p), shellPrompt myXPConfig),
  -- Do not leave useless conky, dzen and xxkb after restart
  ((modm, xK_q), spawn "killall conky dzen2 xxkb; xmonad --recompile; xmonad --restart"),
  -- ResizableTall key bindings
  ((modm, xK_a), sendMessage MirrorShrink),
  ((modm, xK_z), sendMessage MirrorExpand),
  -- Manual page prompt
  ((modm, xK_o), manPrompt myXPConfig),
  ((modm, xK_u), focusUrgent),
  -- Make a screeshot
  ((0,           xK_Print), spawn "scrot -e 'mv $f ~/tmp/'"),
  ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s -e 'mv $f ~/tmp/'") -- interactive
  ]

-- Workspaces names
myWorkspaces = [
  supWsNum "1" "dv",
  supWsNum "2" "wb",
  supWsNum "3" "tr",
  supWsNum "4" "ms",
  supWsNum "5" "dc",
  supWsNum "6" "lg",
  supWsNum "7" "tm",
  supWsNum "8" "",
  supWsNum "9" ""
  ]
  where
    supWsNum wsName wsNum =" " ++ wsName ++  "^p(;_TOP)^fn(" ++ mySmallFont  ++ ")" ++ wsNum ++ "  ^fn()^p()"

-- Dzen config
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = "^bg(" ++ myBgBgColor ++ ")^r(1,15)^bg()",
  ppWsSep = "",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgentWsBgColor,
  ppTitle = (\x -> " " ++ wrapFg myTitleFgColor x),
  ppLayout  = dzenColor myFgColor"" .
                (\x -> case x of
                    "ResizableTall" -> wrapBitmap "rob/tall.xbm"
                    "Mirror ResizableTall" -> wrapBitmap "rob/mtall.xbm"
                    "Full" -> wrapBitmap "rob/full.xbm"
                )
  }
  where
    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
    wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"

-- Define a combination of layouts
myLayoutHook = smartBorders $ (tiled ||| Mirror tiled ||| Full) -- The only window w/o borders
  where
    tiled = ResizableTall nmaster delta ratio []
    nmaster = 1
    delta = 3/100
    ratio = 1/2

-- Urgency hint configuration
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0", "-y", "785", "-h", "15", "-w", "1280",
         "-ta", "r", "-expand", "l",
         "-fg", "" ++ myUrgencyHintFgColor ++ "",
         "-bg", "" ++ myUrgencyHintBgColor ++ "",
         "-fn", "" ++ myFont ++ ""
         ],
      duration = (7 `seconds`)
    }

myManageHook = composeAll [
  resource  =? "XXkb" --> doIgnore
  ]
