-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/loupgaroublonds_xmonad.hs
{-# OPTIONS_GHC -fglasgow-exts #-}

import XMonad
import XMonad.Operations

import XMonad.Actions.Commands
import XMonad.Actions.CycleWS
import XMonad.Actions.DeManage
import qualified XMonad.Actions.DynamicWorkspaces as DW

import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Loggers as LS
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.XSelection

import qualified XMonad.Actions.DwmPromote as DwmP
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.RandomBackground as RandBg
import qualified XMonad.Actions.SinkAll as SinkAll
import qualified XMonad.Actions.TopicSpace as TS
import qualified XMonad.Actions.UpdatePointer as UP
import qualified XMonad.Actions.WithAll as WithAll

import qualified XMonad.Layout.Accordion as Acc
import qualified XMonad.Layout.Circle as Cir
import qualified XMonad.Layout.PerWorkspace as PW
import qualified XMonad.Layout.Tabbed as Tab
import qualified XMonad.Layout.WorkspaceDir as WD

import qualified XMonad.Prompt as P
import qualified XMonad.Prompt.Input as PI
import qualified XMonad.Prompt.Shell as PS
import qualified XMonad.Prompt.Ssh as PSsh
import qualified XMonad.Prompt.Window as PWin
import qualified XMonad.Prompt.Workspace as PWork
import qualified XMonad.StackSet as W

import qualified XMonad.Util.EZConfig as EZ

import Control.Monad
import Data.Either.Utils
import Data.Monoid
import Data.Maybe
import System.IO
import System.Exit

import qualified Data.Map as M

import qualified Network.MPD as MPD

main :: IO ()
main = do
  -- TS.checkTopicConfig _workspaces _topicConfig
  dzen <- spawnPipe "dzen2 -fn '-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*' -ta l"
  xmonad $ defaultConfig
             { borderWidth        = 0
             , terminal           = terminalCmd
             , normalBorderColor  = _normalBorderColor
             , focusedBorderColor = _focusedBorderColor
             , workspaces = _workspaces
             , layoutHook = _layout
             , keys = _keys
             , modMask = mod4Mask
             , logHook = _logHook dzen
	     , handleEventHook = ewmhDesktopsEventHook
             , manageHook = _manageHook }


-- Workspaces

_spaces = M.fromList $
          [ ("schutbord", "~")
          , ("browsen", "~")
          , ("praten", "~")
          , ("muziek", "~/Muziek")
          , ("berichten", "~/Mail")
          , ("agenda", "~/Documenten/Day Planner")
          , ("ldap", "~")
          , ("flim", "~")
          , ("terminals", "~")
          ]

_workspaces = [ "schutbord"]

_topicConfig = TS.TopicConfig {
                 TS.topicDirs = _spaces
               , TS.topicActions = _topicActions
               , TS.defaultTopicAction = (const $ return ())
               , TS.defaultTopic = "schutbord"
               , TS.maxTopicHistory = 10
               }

_topicActions = M.fromList $
                [ ("schutbord", replicateM_ 2 runColourTerminal)
                , ("terminals", replicateM_ 2 runColourTerminal)
                , ("browsen", runBrowser)
                , ("praten", runChat)
                , ("berichten", runMail)
                , ("muziek", runMixer >> runMusicPlayer)
                , ("transmission", runTorrent)
                , ("agenda", runEditor)
                , ("flim", runFilm)
                ]

-- creates the workspace if needed
goto :: TS.Topic -> X ()
goto t = newWorkspace t >> TS.switchTopic _topicConfig t

shift = windows . W.shift


-- Themes

_normalBorderColor :: String
_normalBorderColor = "#EFEFEF"

_focusedBorderColor :: String
_focusedBorderColor= "#000000"

-- Applications

terminalCmd = "urxvtc"
runTerminal :: X()
runTerminal = spawn terminalCmd
runColourTerminal = RandBg.randomBg $ RandBg.HSV 0x44 0x00

saveSession cmd = "/bin/bash -c '" ++ cmd ++ "; /bin/bash'"
manPage cmd = saveSession $ "/usr/bin/man " ++ cmd
inTerminal cmd = terminalCmd ++ " -e " ++ cmd
runInTerminal f = transformPromptSelection f $ terminalCmd ++ " -e "
pasteTerminal = runInTerminal saveSession
manTerminal = runInTerminal manPage

terminalIrssiCmd = terminalCmd
chatCmd = inTerminal irssiCmd
irssiCmd = "ssh -t some.irc.server.com screen -dr irc" -- opens up your irssi right away
runChat = spawn chatCmd

browserCmd = "firefox"
runBrowser = spawn browserCmd
pasteBrowser = safePromptSelection browserCmd

mailCmd = inTerminal "mutt"
runMail = spawn mailCmd

runCmdLine = PS.shellPrompt P.defaultXPConfig

fileManagerCmd = "thunar"
runFileManager = spawn fileManagerCmd

musicPlayerCmd = inTerminal "ncmpc"
runMusicPlayer = spawn musicPlayerCmd
pasteMusicPlayer = promptSelection musicPlayerCmd

mixerCmd = inTerminal "alsamixer"
runMixer = spawn mixerCmd

restartXMonad = broadcastMessage ReleaseResources >>
                restart "xmonad" True

rememberCmd = "/path/to/emacsclient-starter org-protocol:/remember:/t/foo/" -- for adding quick reminders to your agenda
runRemember = spawn rememberCmd

torrentCmd = "transmission"
runTorrent = spawn torrentCmd

filmCmd = "smplayer"
runFilm = spawn filmCmd

-- starter for emacs that has a seperate emacs server per working directory, so the files open for one context are not in the other context
editorCmd :: String
editorCmd = "edit"

runEditor :: X ()
runEditor = spawn editorCmd


-- Keys

_keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X())
_keys = \conf -> EZ.mkKeymap conf $(_emacsKeys conf)

_emacsKeys :: XConfig Layout -> [(String, X())]
_emacsKeys  = \conf ->
              [ -- Applications
                ("M-t", goto "terminals")
              , ("M-S-t", runColourTerminal)
              , ("M-v M-t", pasteTerminal)
              , ("M-v M-d", manTerminal)
              , ("M-i", goto "browsen")
              , ("M-S-i", runBrowser)
              , ("M-v M-i", pasteBrowser)
              , ("M-p", runCmdLine)
              , ("M-x", WD.changeDir P.defaultXPConfig)
              , ("M-e", goto "muziek")
              -- , ("M-o", runMixer)
              , ("M-h", runFileManager)
              , ("M-s", goto "praten")
              , ("M-m", goto "berichten")
              , ("M-S-m", runMail)
              , ("M-u", goto "agenda")
              , ("M-0", goto "schutbord")
              , ("M-w", goto "flim")

              -- mpd
              , ("<XF86AudioPlay>", io $ return . fromRight =<< MPD.withMPD MPD.toggle)
              , ("<XF86AudioStop>", io $ return . fromRight =<< MPD.withMPD MPD.stop)
              , ("<XF86AudioNext>", io $ return . fromRight =<< MPD.withMPD MPD.next)
              , ("<XF86AudioPrev>", io $ return . fromRight =<< MPD.withMPD MPD.previous)
              , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2-")
              , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2+")
              , ("<XF86AudioMute>", spawn "amixer set Master toggle")

              -- couple of scripts to change brightness, very hardware specific to my laptop
              -- brightness
              , ("<XF86MonBrightnessUp>", spawn "lcd-brightness-inc")
              , ("<XF86MonBrightnessDown>", spawn "lcd-brightness-dec")

              -- Layouts
              , ("M-n", refresh)
              , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)
              , ("M-<Space>", sendMessage NextLayout)
              , ("M-<Tab>", windows W.focusDown)
              , ("M-j", windows W.focusDown)
              , ("M-k", windows W.focusUp)
              , ("M-<Return>", windows W.focusMaster)
              , ("M-S-<Return>",  DwmP.dwmpromote)
              , ("M-S-j", windows W.swapDown)
              , ("M-S-k", windows W.swapUp)
              , ("M-g", sendMessage Shrink)
              , ("M-l", sendMessage Expand)
              , ("M-r", withFocused $ windows . W.sink)
              , ("M-,", sendMessage (IncMasterN 1))
              , ("M-.", sendMessage (IncMasterN (-1)))

              -- Toggle full screen
              , ("M-<F12>", sendMessage ToggleStruts >> refresh)

              -- Windows
              , ("M-[", PWork.workspacePrompt P.defaultXPConfig goto)
              , ("M-]", PWin.windowPromptGoto P.defaultXPConfig)
              ,("M-S-[", PWork.workspacePrompt P.defaultXPConfig shift)

              , ("M-c", kill)             -- window
              , ("M-S-c", WithAll.killAll) ] -- window
              -- ++
              -- [ ("M-" ++ [num], goto name)
              --       | (name, num) <-
              --           zip _workspaces (['1' .. '9'] ++ ['0'])]

              -- -- Workspaces
              -- ++
              -- [ ("M-S-" ++ [num], shift name)
              --       | (name, num) <-
              --           zip _workspaces (['1' .. '9'] ++ ['0'])]
              ++
              [ ("M-<Right>", moveTo Next NonEmptyWS)
              , ("M-<Left>", moveTo Prev NonEmptyWS)
              , ("M-S-<Right>", moveTo Next EmptyWS)
              , ("M-S-<Left>", moveTo Prev EmptyWS)
              -- Toggle between current and previous
              , ("M-`", toggleWS)

              , ("M-S-n", PI.inputPrompt P.defaultXPConfig "New Workspace:" PI.?+ newWorkspaceDir)
              , ("M-S-<Backspace>", WithAll.killAll >> DW.removeWorkspace) --buggy, messes with focus and creates flicker, needs to be fixed
              , ("M-S-r", DW.renameWorkspace P.defaultXPConfig)

              -- -- Commands
              -- , ("M-y", runCommand _commands)

              -- -- Remember
              , ("M1-C-r", runRemember)
              -- xmonad
              , ("M1-q", restartXMonad)]

newWorkspace :: WorkspaceId -> X ()
newWorkspace w = do exists <- widExist w
                    if (not exists) then DW.addHiddenWorkspace w else return ()

newWorkspaceDir :: WorkspaceId -> X ()
newWorkspaceDir w = do exists <- widExist w
                       if (not exists)
                           then do DW.addHiddenWorkspace w
                                   goto w
                                   WD.changeDir P.defaultXPConfig
                           else return ()

widExist :: WorkspaceId -> X Bool
widExist wid = do xs <- get
                  return $ widExists wid ( windowset xs )

widExists :: WorkspaceId -> W.StackSet WorkspaceId l a s sd -> Bool
widExists wid ws = wid `elem` map W.tag  (W.workspaces ws)


-- isWorkspace sc w = w `elem` map W.tag (W.current w : W.visible w)

-- Mouse bindings

_mouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
_mouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- Layouts

-- this is by far not finished either
_layout = avoidStruts
          $ _onWorkspace "agenda" Tab.simpleTabbed --scaffolding :)
          $ _onWorkspace "browsen" Tab.simpleTabbed
          $ _onWorkspace "foo" Tab.simpleTabbed
          $ _homeDir

_easyLay = _tiled ||| Mirror _tiled ||| Tab.simpleTabbed

_tiled   = Tall nmaster delta ratio
    where
      nmaster = 1
      ratio   = 1/2
      delta   = 3/100

_onWorkspace t l = PW.onWorkspace t $ WD.workspaceDir (_folderOf t) l

_folderOf = fromMaybe "~" . flip M.lookup _spaces

_homeDir = WD.workspaceDir "~" _easyLay
_homeDirT = WD.workspaceDir "~" Tab.simpleTabbed

-- ManageHooks
_manageHook = composeAll
              [ isFullscreen                  --> doFullFloat
              , className =? "MPlayer"        --> doFloat
              , checkDock --> doIgnore ]
              <+> composeOne
                      [ transience
                      , className =? "Firefox"    -?> doF (W.shift "web")
                      ]
              <+> manageDocks


-- LogHooks

_logHook dzen = do
  DL.dynamicLogWithPP $ DL.defaultPP{ DL.ppOutput = hPutStrLn dzen
                                    , DL.ppExtras = [ LS.logCmd "acpi -b"
                                                    , LS.loadAvg
                                                    , LS.date "%a %b %d %H.%M.%S"
                                                    , LS.logCmd "acpi -t"
                                                    , LS.logCmd "nm-tool |grep State "] }
  ewmhDesktopsLogHook
  UP.updatePointer (UP.Relative 0.9 0.9)


