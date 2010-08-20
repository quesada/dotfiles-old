-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Marcot%27s_xmonad.hs
{-# OPTIONS_GHC -cpp #-}

{-
#include <X11/XF86keysym.h>
-}

import Data.Map hiding (keys, map)

import XMonad

import XMonad.Actions.PerWorkspaceKeys
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Hooks.EventHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks hiding (Direction)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.PerWorkspace

main :: IO ()
main =
  xmonad gnomeConfig
  { workspaces = myWorkspaces
  , layoutHook = myLayoutHook
  , terminal = myTerminal
  , modMask = mod4Mask
  , keys = myKeys
  , manageHook = manageHook gnomeConfig <+> composeAll myManageHook
  }

myWorkspaces :: [String]
myWorkspaces =
  [ "conversa", "terminal", "correio"
  , "navegador", "editor", "noticias"
  , "documentos", "dinheiro", "musica"
  ]

myLayoutHook ::
  PerWorkspace
    (ModifiedLayout AvoidStruts (HandleEvent EwmhDesktopsHook (Choose Tall (Choose Full (Mirror Tall)))))
    (ModifiedLayout AvoidStruts (HandleEvent EwmhDesktopsHook (Choose Full (Choose Tall (Mirror Tall)))))
    a
myLayoutHook =
  onWorkspace "conversa"
  (three (tiled (25 / 34)) Full mirror)
  $ three Full tiled2 mirror

three
  :: (LayoutClass layout1 a, LayoutClass layout2 a, LayoutClass layout3 a)
  => layout1 a -> layout2 a -> layout3 a
  -> ModifiedLayout AvoidStruts (HandleEvent EwmhDesktopsHook (Choose layout1 (Choose layout2 layout3))) a
three first second third = desktopLayoutModifiers $ first ||| second ||| third

tiled :: Rational -> Tall a
tiled = Tall 1 (3 / 100)

mirror :: Mirror Tall a
mirror = Mirror tiled2

tiled2 :: Tall a
tiled2 = tiled $ 1 / 2

myTerminal :: String
myTerminal = "gnome-terminal -e screen"

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf =
  let
  m :: KeyMask
  m = modMask conf

  mS :: KeyMask
  mS = m .|. shiftMask
  in
  union
  (
    fromList $
    [ ((mS, xK_Return), bindOn $ zip myWorkspaces workspaceBindings)
    , shortcut mS "gnome-power-cmd.sh shutdown" xK_s
    ]
    ++ multimedia
  )
  $ keys gnomeConfig conf

workspaceBindings :: [X ()]
workspaceBindings =
  [ spawn "pidgin", spawn myTerminal, spawn "evolution"
  , spawn "epiphany", spawn "emacs -r", spawn "liferea"
  , spawn "evince", spawn "gnucash", spawn "rhythmbox"
  ]

shortcut :: MonadIO x => keyMask -> String -> keySym -> ((keyMask, keySym), x ())
shortcut keyMask command keySym = ((keyMask, keySym), spawn command)

multimedia :: [((KeyMask, KeySym), X ())]
multimedia =
  [ player "prev" XF86XK_AudioPrev
  , player "next" XF86XK_AudioNext
  , amixer "toggle" XF86XK_AudioMute
  , players "pause" "play-pause" XF86XK_AudioPlay
  , player "stop" XF86XK_AudioStop
  , volume '-' XF86XK_AudioLowerVolume
  , volume '+' XF86XK_AudioRaiseVolume
  , noMask "rhythmbox" XF86XK_Music
  ]

player :: String -> keySym -> ((KeyMask, keySym), X ())
player command = players command command

players :: String -> String -> keySym -> ((KeyMask, keySym), X ())
players decibel rhythmbox =
  noMask $ "decibel-audio-player-remote " ++ decibel ++ "; rhythmbox-client --no-start --" ++ rhythmbox

noMask :: String -> keySym -> ((KeyMask, keySym), X ())
noMask = shortcut 0

amixer :: String -> keySym -> ((KeyMask, keySym), X ())
amixer option = noMask $ "amixer set PCM playback" ++ option

volume :: Char -> keySym -> ((KeyMask, keySym), X ())
volume signal = amixer $ "3dB" ++ [signal]

myManageHook :: [ManageHook]
myManageHook =
  [ associate "conversa" "Pidgin"
  , associate "terminal" "Gnome-terminal"
  , associate "correio" "Evolution"
  , associate "navegador" "Epiphany-browser"
  , associate "noticias" "Liferea-bin"
  , associate "documentos" "Evince"
  , editor "BrOffice.org 2.0"
  , editor "Emacs"
  , dinheiro "Gnucash"
  , dinheiro "Glade-3"
  , musica "Rhythmbox"
  , musica "Totem"
  , musica "Decibel-audio-player"
  , musica "Audacity"
  ]

associate :: WorkspaceId -> String -> ManageHook
associate area wmClass = className =? wmClass --> doShift area

editor :: String -> ManageHook
editor = associate "editor"

dinheiro :: String -> ManageHook
dinheiro = associate "dinheiro"

musica :: String -> ManageHook
musica = associate "musica"
