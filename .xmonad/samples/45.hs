-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/void%27s_xmonad.hs
import XMonad hiding ((|||))
import XMonad.ManageHook
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.Promote

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.DwmStyle
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import Data.Ratio ((%))

statusBarCmd= "dzen2 -e '' -w 1000 -ta l -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*' -bg black -fg #d3d7cf "

main = do 
       din <- spawnPipe statusBarCmd
       xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-fn", "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*","-bg", "yellow", "-fg", "black"] } $ defaultConfig
                     { borderWidth        = 2
                     , workspaces         = ["1:main","2:im","3:web","4:mail"] ++ map show [5..9] 
                     , terminal           = "urxvtc"
                     , modMask            = mod4Mask
                     , manageHook         = myManageHook <+> manageHook defaultConfig <+> manageDocks <+> scratchpadManageHookDefault
                     , logHook            = dynamicLogWithPP $ myPP din
                     , layoutHook         = myLayouts
                     }
                     `additionalKeysP` myKeys din

myManageHook  = composeAll [ className =? "Pidgin"         --> doF (W.shift "2:im")
                           , className =? "Firefox"        --> doF (W.shift "3:web")
                           , className =? "Gran Paradiso"  --> doF (W.shift "3:web")
                           , title     =? "mutt"           --> doF (W.shift "4:mail")
                           ]

myKeys conf = [ ("M-<Return>", spawn "urxvtc")
              , ("M-p",        spawn "dmenu_run")
              , ("M-c",        kill)
              -- run programs
              , ("M-f",        spawn "firefox")
              , ("M-e",        spawn "pcmanfm")
              , ("M-s",        scratchpadSpawnActionTerminal "urxvtc")
              -- resize tile
              , ("M-a",        sendMessage MirrorShrink)
              , ("M-z",        sendMessage MirrorExpand)
              -- moving workspaces
              , ("M-<Left>",    prevWS)
              , ("M-<Right>",   nextWS)
              , ("M-S-<Left>",  shiftToPrev)
              , ("M-S-<Right>", shiftToNext)
              , ("M-<Tab>",     toggleWS)

              , ("M-S-<Return>", promote)

              , ("M-u", focusUrgent)
              ]

myPP h = defaultPP 
                 {  ppCurrent = wrap "^fg(#000000)^bg(#a6c292) " " ^fg()^bg()"
                  , ppHidden  = wrap "^i(/home/void/.dzen/has_win_nv.xbm)" " "
                  , ppHiddenNoWindows  = wrap " " " "
                  , ppSep     = " ^fg(grey60)^r(3x3)^fg() "
                  , ppWsSep   = ""
                  , ppLayout  = dzenColor "#80AA83" "" .
                                (\x -> case x of
                                         "Tall"  -> "^i(/home/void/.dzen/tall.xbm)"
                                         "Mirror" -> "^i(/home/void/.dzen/mtall.xbm)"
                                         "Tabs" -> "Tabs"
                                         "IM"  -> "IM"
                                )
                  , ppTitle   = dzenColor "white" "" . wrap "< " " >" 
                  , ppOutput  = hPutStrLn h
                  }

myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }

myLayouts = avoidStruts $ smartBorders $ 
  onWorkspace "2:im" (named "IM" (reflectHoriz $ withIM (1%8) (Title "Buddy List") (reflectHoriz $ dwmStyle shrinkText myTheme tiled ||| (smartBorders $ tabs)))) $ 
  onWorkspace "3:web" (tabs) $ 
  (tiled ||| named "Mirror" (Mirror tiled) ||| tabs)
    where
      tiled = named "Tall" (ResizableTall 1 (3/100) (1/2) [])
      tabs = named "Tabs" (tabbed shrinkText myTheme)
