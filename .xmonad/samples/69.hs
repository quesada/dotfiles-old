-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Wfarr%27s_xmonad.hs
import XMonad
import Control.Monad.Fix

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo

import XMonad.Layout.Magnifier
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Theme

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.Themes

import System.Exit
import System.IO

main = do
  h <- spawnPipe ("dzen2" ++ " " ++ myDzenFlags)
  xmonad $ myUrgencyHook $ defaultConfig
       { terminal           = "urxvtc"
       , borderWidth        = 2
       , normalBorderColor  = "#333333"
       , focusedBorderColor = "#4c7899"
       , modMask            = mod3Mask
       , layoutHook         = myLayout
       , workspaces         = myWorkspaces
       , manageHook         = (manageHook defaultConfig <+> myManageHook) <+> manageDocks
       , logHook            = (dynamicLogWithPP $ myLogHook h) >> updatePointer
       }
       `additionalKeysP` myKeys

myKeys = [ ("M-p", shellPrompt myPrompt)

         , ("M-e",        runOrRaise "emacs" (className =? "Emacs"))
         , ("M-f",        runOrRaise myBrowser (className =? "Firefox"))
         , ("M-<Return>", spawn myTerm)

         , ("M-S-m", windows W.swapMaster)

         , ("M-u", focusUrgent)

         , ("M-b", sendMessage ToggleStruts)

         , ("M-a", sendMessage MirrorShrink)
         , ("M-z", sendMessage MirrorExpand)
           
         , ("M-s g", promptSearch myPrompt myBrowser google)
         , ("M-s h", promptSearch myPrompt myBrowser hoogle)
         , ("M-s i", promptSearch myPrompt myBrowser imdb)
         , ("M-s w", promptSearch myPrompt myBrowser wikipedia)

         , ("M-S-<R>",  moveTo Next NonEmptyWS)
         , ("M-S-<L>",  moveTo Prev NonEmptyWS)

         , ("M-S-t", themePrompt defaultXPConfig)

         , ("M-S-g", gotoMenu)
         , ("M-S-b", bringMenu)

         , ("M-c",   kill1) -- Removes window from ws if possible, else kills
         ]

         ++
         -- This enables view switching, window shifting, and window copying
                  [("M" ++ m ++ ('-':k:[]) , windows $ f i)
                       | (i, k) <- zip myWorkspaces ['1'..'9']
                       , (f, m) <- [(W.view, ""), (W.shift, "-S"), (copy, "-C-S")]]

myPrompt :: XPConfig
myPrompt = defaultXPConfig { font              = "xft:DejaVu Vera Sans Mono:pixelsize=10"
                           , bgColor           = "black"
                           , fgColor           = "#999999"
                           , fgHLight          = "#ffffff"
                           , bgHLight          = "#4c7899"
                           , promptBorderWidth = 0
                           , position          = Bottom
                           , height            = 18
                           , historySize       = 128 }

myTerm, myBrowser :: String
myBrowser = "firefox"
myTerm = "urxvtc"

myDzenFlags  = "-e '' -fn '-*-fixed-medium-r-*--12-*-*-*-*-*-iso8859-1' -y 1186 -w 900 -ta l"
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-bg", "yellow", "-fg", "black"] }

myWorkspaces :: [String]
myWorkspaces = ["1:main", "2:music", "3:emacs", "4:im", "5:email", "6:web" ]
              ++ map show [7 .. 9 :: Int]

myManageHook = composeAll
               [ -- Organized by the workspace they go to
                 className   =? "Nereid"      --> doF(copy "2:music")
               , className   =? "Cowbell"     --> doF(copy "2:music")
               , className   =? "Emacs"       --> doF(copy "3:emacs")
               , className   =? "Empathy"     --> doF(copy "4:im")
               , className   =? "Pidgin"      --> doF(copy "4:im")
               , className   =? "Thunderbird" --> doF(copy "5:email")
               , className   =? "Firefox"     --> doF(copy "6:web")
               ]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
   where
       tiled = ResizableTall nmaster delta ratio []
       nmaster = 1
       ratio = 1/2
       delta = 3/100

myLogHook handle = defaultPP
                   { ppOutput = hPutStrLn handle
                   , ppCurrent = dzenColor "white" "#4c7899" . pad
                   , ppVisible = dzenColor "" "" . pad
                   , ppHidden = dzenColor "" "" . pad
                   , ppUrgent = dzenColor "red" "yellow"
                   , ppTitle = dzenColor "" "" . dzenEscape . pad
                   , ppHiddenNoWindows = const ""
                   , ppWsSep    = "|"
                   , ppSep      = "|"
                   , ppLayout   = dzenColor "" "" .
                                  (\ x -> case x of
                                            "TilePrime Horizontal" -> " TTT "
                                            "TilePrime Vertical"   -> " []= "
                                            "Hinted Full"          -> " [ ] "
                                            _                      -> pad x
                                  )
                   }

