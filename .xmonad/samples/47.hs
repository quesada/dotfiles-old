-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/webframp%27s_xmonad.hs
-----------------------------------------------------------------------
--
-- Module      :  xmonad.hs
-- Copyright   :  (c) Sean Escriva 2008
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  sean.escriva@gmail.com
-- Stability   :  unstable
-- Portability :  not portable,
--
-- customization for the xmonad window manager
-----------------------------------------------------------------------

-- Imports {{{
import XMonad
-- Hooks
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..), focusUrgent)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.SetWMName
-- Prompt
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
-- Actions
import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, toggleWS, WSDirection(..), WSType(..), findWorkspace)
import XMonad.Actions.WindowGo (title, raiseMaybe, runOrRaise) --, (=?))
import XMonad.Actions.UpdatePointer
import qualified XMonad.Actions.Search as S
import XMonad.Actions.GridSelect
-- Util
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.XSelection (safePromptSelection)
import XMonad.Util.EZConfig hiding (additionalMouseBindings, removeMouseBindings)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Util.Loggers
-- Layouts
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.Magnifier (magnifiercz)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.IM
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Named

import System.IO (hPutStrLn)
import Data.Char (isSpace)
import qualified XMonad.StackSet as W
--}}}

-- Main {{{
main :: IO ()
main = do
    dzpipe <- spawnPipe statusBarCmd
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { terminal              = mTerm
        , focusFollowsMouse     = False
        , borderWidth           = 1
        , modMask               = mod4Mask -- win key
        , numlockMask           = mod2Mask
        , workspaces            = ["mail","web","code","irc","term","read","game","art","win"]
        , normalBorderColor     = colorNormalBorder
        , focusedBorderColor    = colorFocusedBorder
        , layoutHook            = avoidStruts $ 
                                  smartBorders (
                                               onWorkspace "term" grids $
                                               onWorkspace "web" mostlyTall $
                                               onWorkspace "art" gimp standardLayouts)
        , manageHook            = mManageHook -- <+> manageMonitor screenletRingSensor
        , logHook               = (dynamicLogWithPP $ mPP dzpipe) >> updatePointer (Relative 0.95 0.95)
        , startupHook           = return () >> checkKeymap defaultConfig lacKeys >> setWMName "LG3D"
        }
        `additionalKeysP` lacKeys
-- }}}

-- Config {{{
-- single options here, more complex settings(by comparison) get separated
-- }}}

-- Theme {{{
-- Color names are easier to remember:
colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"

colorNormalBorder    = "#1c2636"
colorFocusedBorder   = "#2797d8"
barFont  = "terminus"
barXFont = "inconsolata:size=14"
xftFont = "xft: inconsolata-14"
--}}}
statusBarCmd = "dzen2" ++
               " -bg '" ++ colorDarkGray ++ "'" ++
               " -fg '" ++ colorBlue ++ "'" ++
               " -sa c" ++
               " -fn '" ++ barXFont ++ "'" ++
               " -w 1920 -x 0 -y 0 -ta l -e ''"

notesFile = "/home/webframp/TODO"
mTerm     = "urxvtc"

-- Custom Searches
enro40, roen40, hayoo :: S.SearchEngine
enro40 = S.searchEngine "enro40" "http://dictionare.com/phpdic/enro40.php?field0="
roen40 = S.searchEngine "roen40" "http://dictionare.com/phpdic/roen40.php?field0="
hayoo = S.searchEngine "hayoo" "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="

-- Pretty printer {{{
-- dynamiclog pretty printer for dzen
mPP h = defaultPP
        { ppCurrent = wrap ("^fg(" ++ colorOrange ++ ")^bg(" ++ colorDarkGray ++ ")^p(2)") "^p(2)^fg()^bg()"
        , ppVisible = wrap ("^fg(" ++ colorBlue ++ ")^bg(" ++ colorDarkGray ++ ")^p(2)") "^p(2)^fg()^bg()"
        , ppSep     = " ^fg(grey60)^r(1x8)^fg() "
        , ppLayout  = dzenColor colorWhite "" . (\x -> case x of
                                                            "Mirror Tall"                               -> pad "^i(/home/webframp/.xmonad/dzen/mtall.xbm)"
                                                            "ResizableTall"                             -> pad "^i(/home/webframp/.xmonad/dzen/tall.xbm)"
                                                            "Full"                                      -> pad "^i(/home/webframp/.xmonad/dzen/full.xbm)"
                                                            "Magnifier GridRatio 1.3333333333333333"    -> pad "^i(/home/webframp/.xmonad/dzen/mgrid.xbm)"
                                                            "GridRatio 1.3333333333333333"              -> pad "^i(/home/webframp/.xmonad/dzen/grid.xbm)"
                                                            "ReflectX Gimp"                             -> pad "^i(/home/webframp/.xmonad/dzen/reflectx.xbm)"
                                                            _                                           -> pad x
                                                   )
        , ppUrgent  = dzenColor colorDarkGray colorYellow . wrap "[" "]"
        , ppTitle   = dzenColor colorWhite "" . trim
        , ppExtras  = [logMail]
        , ppOutput  = hPutStrLn h
        }
    where
        logMail = dzenColorL colorPink "" . wrapL mailIcon "". padL $ maildirNew mailDir
        mailIcon = "^i(/home/webframp/.dzen/icons/dzen_bitmaps/envelope.xbm)"
        mailDir = "/home/webframp/.mail/GMAIL/INBOX"

--}}}

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 14
                    , historyFilter         = deleteConsecutive
                    }

largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 20
                }
-- }}}

-- Layout Hook{{{
standardLayouts = Mirror tiled  |||
                  defaultTall   |||
                  Full
                where
                  tiled       = Tall nmaster delta ratio
                  defaultTall = ResizableTall 1 (3/100) (1/2) []
                  nmaster     = 1
                  ratio       = toRational (2/(1 + sqrt 5 :: Double)) -- golden, thx Octoploid
                  delta       = 0.03

grids = magnifiercz 1.2 (GridRatio (4/3))  |||
        GridRatio (4/3)

mostlyTall = ResizableTall 1 (3/100) (1/2) [] ||| Full
gimp     = reflectHoriz $ 
           named "Gimp" $ 
           withIM (11/64) (Role "gimp-toolbox") $ 
           ResizableTall 2 (1/118) (11/20) [5/4,5/4,5/4]
--}}}
-- Window rules aka Manage Hook {{{
mManageHook :: ManageHook
mManageHook = scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35) <+>
               (composeAll . concat $
    [ [ className =? c --> doFloat        | c <- floats ],
      [ className =? w --> doShift "web"  | w <- webs]   ,
      [ className =? g --> doShift "game" | g <- games]  ,
      [ className =? m --> doShift "mail" | m <- mails]  ,
      [ resource  =? "desktop_window"  --> doIgnore
      , className =? "Apvlv"           --> doShift "read"
      , className =? "VirtualBox"      --> doShift "win"
      , className =? "Tsclient"        --> doShift "win"
      , className =? "Vncviewer"       --> doShift "win"
      , className =? "Gimp"            --> doShift "art"
      , title     =? "irssi"           --> doShift "irc"
      , title     =? "mutt"            --> doShift "mail"
      , title     =? "Save a Bookmark" --> doCenterFloat
      , isFullscreen                   --> doFullFloat
      , isDialog                       --> doCenterFloat
      ] ])
        <+> manageDocks -- make some space
            where floats = ["Mplayer","Tsclient","VirtualBox","Gtklp","smc"]
                  webs   = ["Navigator","Gran Paradiso","Firefox", "Midori", "Minefield"]
                  games  = ["roguestar-gl","neverputt","neverball","wesnoth"]
                  mails  = ["Evolution", "Thunderbird-bin", "Shredder"]
-- }}}
lacKeys :: [([Char], X ())]
lacKeys =
    [ ("M-p"        , runOrRaisePrompt largeXPConfig )
    , ("M-S-p"      , spawn "exe=`dmenu_path | dmenu -b` \"exec $exe\"") -- backup launcher
    , ("M-g"        , runOrRaise "firefox-nightly" (className =? "Minefield" <||> className =? "Gran Paradiso"))
    , ("M-u"        , runOrRaise "uzbl" (className =? "Uzbl" ))
    , ("M-S-g"      , safePromptSelection "firefox-nightly")
    , ("M-w"        , goToSelected defaultGSConfig)
    , ("M-C-n"      , appendFilePrompt largeXPConfig { bgColor = colorOrange, fgColor = colorDarkGray } notesFile)
    , ("M-S-z"      , safeSpawn "mocp" ["-G"])                -- play/pause
    , ("M-S-,"      , safeSpawn "mocp" ["-r"])                -- rev
    , ("M-S-."      , safeSpawn "mocp" ["-f"])                -- fwd
    , ("M-<Esc>"    , focusUrgent)
    , ("M-`"        , scratchpadSpawnAction defaultConfig { terminal = mTerm })  -- scratchpad
    , ("M-S-l"      , unsafeSpawn "slock")                  -- screen lock
    , ("M-i"    , raiseMaybe (runInTerm "-title irssi" "sh -c 'screen -D -R -S irc irssi'") (title =? "irssi"))
    , ("M-S-i"  , raiseMaybe (runInTerm "-title irssi" "sh -c 'ssh -t webframp@astrotrain screen -D -R -S irc irssi'") (title =? "irssi"))
    , ("M-m"    , raiseMaybe (runInTerm "-title mutt" "sh -c 'screen -D -R -S mail mutt'") (title =? "mutt"))
    , ("M-S-m"  , runOrRaise "thunderbird3" (className =? "Shredder"))
    , ("M-S-e"  , raiseMaybe (runInTerm "-title files" "sh -c 'screen -D -R -S files vifm'") (title =? "files"))
    , ("M-C-e"    , safeSpawn "pcmanfm" ["--no-desktop"])
    , ("M-v"    , raiseMaybe (spawn "vncviewer -passwd ~/.vpasswd 10.0.2.163") (className =? "Vncviewer"))
    , ("M-b"    , sendMessage ToggleStruts)
    -- focus NonEmpty wss except scratchpad
    , ("M-s", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next NonEmptyWS 1)
    , ("M-d", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev NonEmptyWS 1)
    -- move window to NonEmpty wss except scratchpad
    , ("M-S-s", windows . W.shift =<< findWorkspace getSortByIndexNoSP Next NonEmptyWS 1)
    , ("M-S-d", windows . W.shift =<< findWorkspace getSortByIndexNoSP Prev NonEmptyWS 1)
    -- move window to and focus NonEmpty wss except scratchpad
    , ("M-C-s"    , shiftAndView Next)
    , ("M-C-d"    , shiftAndView Prev)
    , ("M-f"      , nextScreen)
    , ("M-a"      , prevScreen)
    , ("M-S-f"    , shiftNextScreen)
    , ("M-S-a"    , shiftPrevScreen)
    , ("M-<Tab>"  , toggleWS)
    -- Media keys
    , ("<XF86AudioLowerVolume>" , unsafeSpawn "amixer -q set Master 2dB-" 	)
    , ("<XF86AudioMute>"        , unsafeSpawn "amixer -q set Master toggle")
    , ("<XF86AudioRaiseVolume>" , unsafeSpawn "amixer -q set Master 2dB+" 	)
    -- Screenshot
    , ("<Print>" , unsafeSpawn "scrot '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/screenshots/'")
    ]
    ++
    -- Search methods
    -- mapped to mod-c for 'a căuta'
    -- FIXME: broken after recent 'safeSpawn' String -> [String] changes
    [("M-c " ++ k, S.promptSearch largeXPConfig f) | (k,f) <- searchList ]
    ++
    [("M-C-c " ++ k, S.selectSearch f) | (k,f) <- searchList ]
    where -- | non-empty workspaces less scratchpad
        shiftAndView dir = findWorkspace getSortByIndexNoSP dir NonEmptyWS 1
                >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
        getSortByIndexNoSP =
                fmap (.scratchpadFilterOutWorkspace) getSortByIndex

searchList :: [([Char], S.SearchEngine)]
searchList = [ ("g", S.google)
             , ("h", hayoo)
             , ("i", S.isohunt)
             , ("w", S.wikipedia)
             , ("d", S.dictionary)
             , ("t", S.thesaurus)
             , ("a", S.amazon)
             , ("y", S.youtube)
             , ("e", enro40)
             , ("r", roen40)
             ]

-- vim:foldmethod=marker sw=4 sts=4 ts=4 tw=0 et ai nowrap
