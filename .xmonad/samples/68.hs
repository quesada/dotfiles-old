-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/sphynx%27s_xmonad.hs
--
-- dying_sphynx's xmonad config file.
--

import XMonad hiding ((|||))

import System.Exit
import System.IO
import Data.Ratio ((%))

import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow
import XMonad.Actions.DwmPromote
import XMonad.Actions.FloatKeys
import XMonad.Actions.NoBorders
import XMonad.Actions.Search
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

------------------------------------------------------------------------
-- workspaces

myWorkspaces = ["term","web","emacs","im","media","dev","read","misc","skype"]

------------------------------------------------------------------------
-- keybindings in emacs-style

emacsStyleKeys =
     [ ("M-<Backspace>", focusUrgent)
     , ("M-S-q", spawn "xmessage 'use M-S-F12 to quit xmonad :)'")
     , ("M-S-<F12>", io (exitWith ExitSuccess))  
     , ("M-<R>", windows W.focusDown)
     , ("M-S-<R>", windows W.swapDown)
     , ("M-<L>", windows W.focusUp)
     , ("M-S-<L>", windows W.swapUp)
     , ("M-<Return>", dwmpromote)
     , ("M-x 0", kill)
     , ("M-x 1", sendMessage $ JumpToLayout "Full")
     , ("M-f", sendMessage ToggleLayout)
     , ("M-b", sendMessage ToggleStruts)
     , ("M-x s", sshPrompt ownXPConfig)
     , ("M-v", windows copyToAll)
     , ("M-S-v", killAllOtherCopies)
     , ("M-s", spawn "urxvtc -title scratchpad")
     , ("M-x g", promptSearch ownXPConfig google)
     , ("M-S-x g", selectSearch google)
     , ("M-x w", promptSearch ownXPConfig wikipedia)
     , ("M-S-x w", selectSearch wikipedia)
     , ("M-x e", promptSearch ownXPConfig lingvoEnRu)
     , ("M-S-x e", selectSearch lingvoEnRu)
     ]
     
-----------------------------------------------------------------------
-- search engines

lingvoEnRu = simpleEngine "http://lingvo.yandex.ru/en?lang=en&search_type=lingvo&st_translate=1&text="

------------------------------------------------------------------------
-- prompt config

ownXPConfig :: XPConfig
ownXPConfig = defaultXPConfig
              { font              = "xft:Terminus:pixelsize=14"
              , bgColor           = "#3f3c6d"
              , fgColor           = "#a8a3f7"
              , fgHLight          = "#a8a3f7"
              , bgHLight          = "blue"
              , borderColor       = "#FFFFFF"
              }

------------------------------------------------------------------------
-- window rules

myManageHook = scratchpadManageHookDefault <+>
               (composeAll . concat $
    [ [ className =? c --> doFloat | c <- floats] ,
      [ className =? w --> moveTo "web" | w <- webs] ,
      [ resource  =? "desktop_window" --> doIgnore
      , className =? "urxvtc"         --> moveTo "term"
      , className =? "Emacs"          --> moveTo "emacs"
      , className =? "Pidgin"         --> moveTo "im"
      , className =? "Exaile.py"      --> moveTo "media"
      , className =? "Totem"          --> moveTo "media"
      , className =? "Skype"          --> moveTo "skype"
      , className =? "Epdfview"       --> moveTo "read"
      , className =? "Eclipse"        --> moveTo "dev"
      ] ])
      where floats = ["MPlayer", ".", "feh"]
            webs   = ["Firefox-bin", "Firefox", "Minefield"]
            moveTo = doF . W.shift

------------------------------------------------------------------------
-- status bar and logging

myPP statusPipe = xmobarPP {
    ppOutput  = hPutStrLn statusPipe,
    ppTitle   = xmobarColor "green"  "" . shorten 50,
    ppUrgent  = xmobarColor "red" "" . wrap "!" "!"
}

myLogHook = dynamicLogWithPP . myPP

------------------------------------------------------------------------
-- layouts

imLayout = avoidStruts $
           IM (1%7) (Or (And (ClassName "Pidgin") (Role "buddy_list"))
                        (And (ClassName "Skype")  (And (Role "") (Not (Title "Options")))))

tabbedLayout = tabbed shrinkText tabbedConf

tabbedConf = defaultTheme {
    fontName = "xft:Terminus"
}

genericLayouts = avoidStruts $
                 smartBorders $
                 toggleLayouts (noBorders Full) $
                 tiled ||| tabbedLayout ||| Mirror tiled ||| (noBorders Full)
  where
    tiled = Tall 1 (3 / 100) (1 / 2)

myLayouts = onWorkspaces ["im", "skype"] imLayout $
            genericLayouts

----------------------------------------------------------------------
-- config itself

sphynxConfig statusPipe = defaultConfig {
    terminal           = "urxvtc",
    modMask            = mod4Mask,
    numlockMask        = mod2Mask,
    workspaces         = myWorkspaces,
    borderWidth        = 2,
    normalBorderColor  = "#454545",
    focusedBorderColor = "#f9b857",
    focusFollowsMouse  = False,
    manageHook         = myManageHook <+> manageDocks,
    logHook            = myLogHook statusPipe,
    layoutHook         = myLayouts
}

main = do statusPipe <- spawnPipe "xmobar"
          xmonad $
            withUrgencyHook NoUrgencyHook $
            sphynxConfig statusPipe `additionalKeysP` emacsStyleKeys

