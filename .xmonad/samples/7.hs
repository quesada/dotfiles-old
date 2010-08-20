-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/adamvo%27s_xmonad.hs
-- current darcs as of 2009-12-05
{-# OPTIONS_GHC -W -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, FlexibleInstances, PatternGuards #-}

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Control.Monad.Instances ()

-- Update these with:  ghc -ddump-minimal-imports
import Control.Arrow((***))
import Control.Applicative((<*), liftA2)
import Control.Monad(Monad(return, (>>=), (>>)), Functor(..), (=<<), mapM, sequence, (<=<), zipWithM_)
import Data.Function((.), const, ($), flip)
import Data.IORef(newIORef)
import Data.List((++), filter, zip, map, concatMap, length, repeat, zipWith, unwords, isPrefixOf, intercalate, nub)
import Data.Maybe(catMaybes, fromMaybe, listToMaybe)
import Data.Monoid(Monoid(mconcat))
import Graphics.X11.Xinerama(getScreenInfo)
import System.IO(IO, Handle, hPutStrLn)
import XMonad.Actions.DwmPromote(dwmpromote)
import XMonad.Actions.FloatSnap(Direction2D(..), snapGrow, snapMove, snapShrink)
import XMonad.Actions.GridSelect(gridselect, GSConfig(gs_navigate), defaultGSConfig, goToSelected)
import XMonad.Actions.Search(mathworld, wikipedia, multi, promptSearch)
import XMonad.Actions.SpawnOn -- (manageSpawn, mkSpawner, shellPromptHere, spawnOn)
import XMonad.Actions.Submap(submap)
import XMonad.Actions.TopicSpace(TopicConfig(..), Topic, (>*>), checkTopicConfig, currentTopicAction, currentTopicDir, pprWindowSet, shiftNthLastFocused, switchNthLastFocused, switchTopic)
import XMonad.Actions.UpdatePointer(PointerPosition(TowardsCentre), updatePointer)
import XMonad.Actions.Warp(warpToScreen)
import XMonad.Hooks.DynamicLog(PP(ppOrder, ppTitle, ppLayout, ppVisible, ppHidden, ppCurrent, ppSep), dynamicLogString, defaultPP, xmobarColor, sjanssenPP)
import XMonad.Hooks.EwmhDesktops(ewmh)
import XMonad.Hooks.ManageDocks(ToggleStruts(ToggleStruts), avoidStruts, manageDocks)
import XMonad.Hooks.ManageHelpers(doFullFloat, isFullscreen)
import XMonad.Hooks.UrgencyHook(FocusHook(..), withUrgencyHook)
import XMonad.Layout.BoringWindows(boringAuto, focusDown, focusUp)
import XMonad.Layout.IM(Property(Role), withIM)
import XMonad.Layout.LayoutHints(layoutHintsToCenter)
import XMonad.Layout.LayoutScreens(layoutScreens)
import XMonad.Layout.Mosaic(Aspect(Wider, Taller), mosaic)
import XMonad.Layout.MouseResizableTile(mouseResizableTile)
import XMonad.Layout.Named(named)
import XMonad.Layout.NoBorders(Ambiguity(Screen), lessBorders, noBorders)
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.SubLayouts(GroupMsg(UnMergeAll, UnMerge, MergeAll), defaultSublMap, onGroup, pullGroup, pushWindow, subLayout)
import XMonad.Layout.Tabbed(defaultTheme, addTabs, fontName, shrinkText)
import XMonad.Layout.TwoPane(TwoPane(..))
import XMonad.Layout.WindowNavigation(Navigate(Swap, Go), configurableNavigation, navigateColor)
import XMonad.Prompt(XPConfig(font), XPrompt(showXPrompt), greenXPConfig, mkXPrompt)
import XMonad.Prompt.Input (inputPrompt)
import XMonad.Prompt.RunOrRaise(runOrRaisePrompt)
import XMonad.Prompt.Ssh(sshPrompt)
import XMonad.Prompt.Window(windowPromptGoto)
import XMonad.Prompt.XMonad(xmonadPrompt)
import XMonad.Util.EZConfig(additionalKeysP, checkKeymap)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.StringProp(setStringProp, getStringProp)

import XMonad.Util.Replace (replace)
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.WindowArranger
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.SimplestFloat

import XMonad.Layout.LimitWindows
import Data.List
import Control.Monad

import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Magnifier
import Data.Monoid

import XMonad.Layout.SimplestFloat
import XMonad.Layout.IM

import XMonad.Layout.Spacing

import Data.Maybe
import Control.Monad.Writer

import qualified XMonad.Util.ExtensibleState as XS
import System.Process

import XMonad.Util.Paste
import XMonad.Layout.Spiral as Spiral

import XMonad.Layout.MosaicAlt

main :: IO ()
main = do
    replace
    checkTopicConfig myTopics myTopicConfig
    xmonad . ewmh . withUrgencyHook FocusHook . myConfig =<< mapM xmobarScreen =<< getScreens

sofficeToolbox = className =? "OpenOffice.org 3.1" <&&> isInProperty "WM_PROTOCOLS" "WM_TAKE_FOCUS"

logCrossingEvs e@(CrossingEvent { }) = trace (show e) >> return (All True)
logCrossingEvs _ = return (All True)

isMaster = gets $ maybe True (null . W.up) . W.stack . W.workspace . W.current . windowset
lh = isMaster >>= flip unless (windows W.shiftMaster)

myConfig hs = let c = defaultConfig {
      layoutHook = myLayout
    , focusFollowsMouse = False
    , focusedBorderColor = "#ff0000"
    , startupHook = do
        return ()
        checkKeymap (myConfig []) (myKeys c)
    , terminal = "urxvt"
    , modMask = mod4Mask
    , logHook = do
        multiPP'
            (mergePPOutputs [pprWindowSet myTopicConfig,dynamicLogString . onlyTitle])
            myPP
            myPP { ppTitle = const "" }
            hs
        updatePointer (TowardsCentre 0.2 0.2)
    , handleEventHook = focusFollow
    , workspaces = myTopics
    , manageHook = composeAll
                    [manageSpawn
                    ,isFullscreen --> doFullFloat
                    ,manageDocks
                     -- ,sofficeToolbox -->  ((ask >>= doF . W.sink)) -- (ask >>= \w -> liftX (asks display >>= io . flip raiseWindow w) >> doIgnore)
                    ]
    } in additionalKeysP c (myKeys c)

myXPConfig :: XPConfig
myXPConfig = greenXPConfig { font = "xft:Profont:pixelsize=15:autohint=true" }

gsConfig = defaultGSConfig { gs_navigate = neiu `M.union` gs_navigate (defaultGSConfig`asTypeOf`gsConfig) }
    where neiu = M.insert (0,xK_space) (const (0,0)) $ M.map (\(x,y) (a,b) -> (x+a,y+b)) $ M.fromList
            [((0,xK_n),(-1,0))
            ,((0,xK_e),(0,1))
            ,((0,xK_i),(1,0))
            ,((0,xK_u),(0,-1))]

-------------------- Layout ----------------------------------
myLayout = avoidStruts
         . onWorkspace "325" (spacing 10 $ configurableNavigation (navigateColor "#ffff00") $ Spiral.spiralWithDir Spiral.West Spiral.CW (4/5))
         . onWorkspace "imtest" (spacing 5 m)
         . onWorkspace "floatTest" (named "F" $ borderResize $ noFrillsDeco shrinkText defaultTheme $ windowArrange simplestFloat)
         . onWorkspace "gimp" (named "G" $ gimp)
         . onWorkspace "movie" (magnifier m)
         $ m ||| named "F" (noBorders Full)
    where nav = configurableNavigation (navigateColor "#ffff00")
          m = named "M"
            . lessBorders Screen
            . layoutHintsToCenter
            . addTabs shrinkText defaultTheme
            . nav
            . boringAuto
            . subLayout [] (Simplest ||| simplestFloat)
            $ mosaic 1.5 [7,5,2]
            -- $ MosaicAlt (Rectangle 0 0 1280 780) (W.Stack 0 [] []) M.empty
          gimp = withIM 0.11 (Role "gimp-toolbox")
               . withIM 0.15 (Role "gimp-dock")
               . addTabs shrinkText defaultTheme
               . nav
               . boringAuto
               . subLayout [] Simplest
               $ mouseResizableTile ||| Full
--------------------------------------------------------------
-------------------- Keys ------------------------------------
myKeys c =
    [("M-<Left>"   , withFocused $ snapMove L Nothing  )
    ,("M-<Right>"  , withFocused $ snapMove R Nothing  )
    ,("M-<Up>"     , withFocused $ snapMove U Nothing  )
    ,("M-<Down>"   , withFocused $ snapMove D Nothing  )
    ,("M-S-<Left>" , withFocused $ snapShrink R Nothing)
    ,("M-S-<Right>", withFocused $ snapGrow   R Nothing)
    ,("M-S-<Up>"   , withFocused $ snapShrink D Nothing)
    ,("M-S-<Down>" , withFocused $ snapGrow   D Nothing)

    , ("M-l", withFocused (sendMessage . expandWindowAlt) >> sendMessage Expand)
    , ("M-h", withFocused (sendMessage . shrinkWindowAlt) >> sendMessage Shrink)

    ,("M-;", withFocused (sendMessage . tallWindowAlt) >> sendMessage Taller)
    ,("M-o", withFocused (sendMessage . wideWindowAlt) >> sendMessage Wider )

    ,("M-v", toggleFF)

    -- ,("M-d", layoutScreens 2 $ TwoPane 0.5 0.5)
    -- ,("S-M-d", rescreen)
    ,("M-S-b", restart "/home/aavogt/bin/obtoxmd" True)
    ,("M-S-d", restart "urxvt -e xmonad" False)

    ,("M-d",  pasteString "?><://\\{}[]" )

    ,("M-S-o"  , withFocused $ sendMessage . UnMerge   )
    ,("M-S-C-o", withFocused $ sendMessage . UnMergeAll)
    ,("M-C-m"  , withFocused $ sendMessage . MergeAll  )
    ,("M-C-."  , onGroup W.focusDown')
    ,("M-C-,"  , onGroup W.focusUp'  )

    ,("M-p",  shellPromptHere myXPConfig)
    ,("M-x", submap $ M.fromList subMaps)
    ,("M-g", submap $ defaultSublMap c  )

    ,("M-S-.", focusDown)
    ,("M-S-,", focusUp  )

    ,("M-S-a", currentTopicAction myTopicConfig)
    ,("M-a", warpToCentre >> goToSelected gsConfig)
    -- swap among non-visible topics, regardless of the number of screens I'm using
    ,("M-<Tab>", switchNthLastFocused myTopicConfig . succ . length . W.visible . windowset =<< get )

    ,("M-s"  , warpToCentre >> promptedGoto )
    ,("M-S-s", warpToCentre >> promptedShift)

    ,("M-b", sendMessage ToggleStruts)
    ,("M-<Return>", dwmpromote)
    ,("M-S-<Return>", spawnShell)
    -- don't force a recompile, if nothing has changed (xmonad --recompile runs XMonad.recompile True)
    ,("M-q", spawn $ "ghc -e 'XMonad.recompile False >>= flip Control.Monad.unless System.Exit.exitFailure'"
                            ++ "&& xmonad --restart")
    ,("M-S-q", spawn "~/wip/x11-wm/xmonad/rebuild.sh")
    ,("<Print>",  spawn "scrot")
    ]
    ++
    concatMap (\(m,f) -> lrud ("M-"++m) f)
        [("S-"  , sendMessage . Swap)
        ,("C-"  , sendMessage . pullGroup)
        ,("S-C-", sendMessage . pushWindow)
        ,(""    , sendMessage . Go)]
    ++ mediaKeys ++
    [("M-"++m++[key], screenWorkspace sc >>= flip whenJust (windows . f))
        | (f, m) <- [(W.view, ""), (W.shift, "S-")]
        , (key, sc) <- zip "wf" [0 .. ]]
    ++
    [ ("M-"++m++[k], a i)
        | (a, m) <- [(switchNthLastFocused myTopicConfig,""),(shiftNthLastFocused, "S-")]
        , (i, k) <- zip [1..] "123456789"]

-- helper for windowNavigation keys
--    note: with colemak neiu are placed where jkli are with qwerty layout
lrud :: String -> (Direction2D -> b) -> [(String, b)]
lrud m cmd = zip ks cmds
    where
      ks   = map (\x -> m ++ [x]) "niue"
      cmds = zipWith ($) (repeat cmd) [L,R,U,D]

subMaps = [((0, xK_o),  runOrRaisePrompt myXPConfig),
           ((0, xK_p),  shellPromptHere myXPConfig),
           ((0, xK_x), xmonadPrompt myXPConfig),
           ((0, xK_z), sshPrompt myXPConfig),
           ((shiftMask, xK_w), windowPromptGoto myXPConfig),
           ((0, xK_w), promptSearch myXPConfig wikipedia),
           ((0, xK_s), promptSearch myXPConfig multi),
           ((0, xK_m), promptSearch myXPConfig mathworld),
           ((0, xK_b), sendMessage ToggleStruts),
           ((0, xK_f), withFocused $ windows . W.sink),
           ((0, xK_v), refresh),
           ((0, xK_c), asks config >>= spawnHere . terminal),
           ((0, xK_k), kill)
           ]

mediaKeys = [("<XF86AudioPlay>", mpcAct "toggle"),
             ("<XF86AudioStop>", promptHost),
             ("<XF86AudioNext>", mpcAct "next"),
             ("<XF86AudioPrev>", mpcAct "prev"),
             ("<XF86AudioMute>", spawn "ossmix vmix0-outvol 0"),
             ("S-<XF86AudioMute>", spawn "~/bin/speakers.sh"),
             ("<XF86AudioLowerVolume>",   spawn "ossmix vmix0-outvol -- -1"),
             ("S-<XF86AudioLowerVolume>", spawn "ossmix vmix0-outvol -- -0.1"),
             ("<XF86AudioRaiseVolume>",   spawn "ossmix vmix0-outvol +1"),
             ("S-<XF86AudioRaiseVolume>", spawn "ossmix vmix0-outvol +0.1"),
             ("<XF86Sleep>", spawn "sudo pm-suspend")]
    where mpcAct c = do
            h <- XS.gets hostPrompt
            spawn $ unwords ["export MPD_HOST="++h,";","mpc",c]

-- Prompt for mpd host
newtype HostPrompt = HostPrompt { hostPrompt :: String } deriving (Read,Show,Typeable)
instance ExtensionClass HostPrompt where
    initialValue = HostPrompt "localhost"
    extensionType = PersistentExtension

instance XPrompt HostPrompt where showXPrompt _ = "Pick MPD Host: "
promptHost = mkXPrompt (HostPrompt "") myXPConfig (return . compl) (XS.put . HostPrompt)
    where compl s = nub $ filter (s `isPrefixOf`) ["localhost","dell"]
--------------------------------------------------------------

warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5

-------------------- Support for per-screen xmobars ---------
-- Some parts of this should be merged into contrib sometime
getScreens :: IO [Int]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
    where f = fmap (zipWith const [0..]) . getScreenInfo

multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> [Handle] -- ^ Handles for the status bars, in order of increasing X
                    -- screen number
        -> X ()
multiPP = multiPP' dynamicLogString

multiPP' :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
multiPP' dynlStr focusPP unfocusPP handles = do
    state <- get
    let pickPP :: WorkspaceId -> WriterT (Last XState) X String
        pickPP ws = do
            let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset state
            put state{ windowset = W.view ws $ windowset state }
            out <- lift $ dynlStr $ if isFoc then focusPP else unfocusPP
            when isFoc $ get >>= tell . Last . Just
            return out
    flip whenJust put . getLast
        =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
        =<< mapM screenWorkspace (zipWith const [0..] handles)

mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

onlyTitle :: PP -> PP
onlyTitle pp = defaultPP { ppCurrent = const ""
                         , ppHidden = const ""
                         , ppVisible = const ""
                         , ppLayout = ppLayout pp
                         , ppTitle = ppTitle pp }

-- | Requires a recent addition to xmobar (>0.9.2), otherwise you have to use
-- multiple configuration files, which gets messy
xmobarScreen :: Int -> IO Handle
xmobarScreen = spawnPipe . ("~/.cabal/bin/xmobar -x " ++) . show

myPP :: PP
myPP = sjanssenPP { ppLayout = xmobarColor "orange" "" }
--------------------------------------------------------------

-------------------- X.Actions.TopicSpace --------------------
myTopics :: [Topic]
myTopics =
  [ "dashboard"
  , "web"
  , "haskell"
  , "irc"
  , "admin"
  , "documents"
  , "gimp"
  , "gitit"
  , "mail"
  , "movie"
  , "music"
  , "pdf"
  , "xmonad-conf"
  , "xmonad-contrib"
  , "xmonad-extras"
  , "xmonad"
  , "xmobar"
  , "wip"
  , "floatTest"
  , "imtest"
  , "325"
  , "test"
  ]


myTopicConfig = TopicConfig
  { topicDirs = M.fromList $
      [ ("dashboard", "./")
      , ("haskell", "haskell")
      , ("xmonad-conf", ".xmonad")
      , ("xmonad-extras", "wip/x11-wm/xmonad/extras/xmonad-extras/XMonad")
      , ("xmonad", "wip/x11-wm/xmonad/core/xmonad")
      , ("xmonad-contrib", "wip/x11-wm/xmonad/contrib/XMonadContrib/XMonad")
      , ("xmobar", "wip/x11-wm/xmobar")
      , ("movie", "media/movie")
      , ("music", "media/music")
      , ("documents", "doc")
      , ("pdf", "ref")
      , ("gitit", "wip/gitit")
      , ("gimp", "./")
      , ("wip", "wip")
      ]
  , defaultTopicAction = const $ spawnShell >*> 2
  , defaultTopic = "dashboard"
  , maxTopicHistory = 10
  , topicActions = M.fromList $
      [ ("xmonad-conf", spawnShellIn ".xmonad/lib/XMonad/Layout" >>
                        spawn "urxvt -e vim ~/.xmonad/xmonad.hs")
      , ("xmonad-contrib", spawnShell >*> 2)
      , ("xmobar",     spawnShellIn "wip/x11-wm/xmobar/Plugins" >*> 2)
      , ("music",      spawn "urxvt -e ncmpc" >> spawn "export MPD_HOST=192.168.1.2; mpc && urxvt -e ncmpc -h 192.168.1.2")
      , ("mail",       spawnOn "mail" "urxvt -e mutt")
      , ("irc",        spawnOn "irc" "urxvt --title irc -e ssh engage")
      , ("web",        spawnOn "web" "firefox")
      , ("pdf",        spawnOn "pdf" "okular")
      , ("gimp",       spawnHere "gimp")
      ]
  }

-- From the sample config in TopicSpace, these should probably be exported from that module
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn dir = do
    -- color <- randomBg' (HSV 255 255)
    t <- asks (terminal . config)
    spawnHere $ "cd " ++ dir ++ " && " ++ t -- ++ " -bg " ++ color

wsgrid = gridselect gsConfig <=< asks $ map (\x -> (x,x)) . workspaces . config

promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)

promptedShift = wsgrid >>= \x -> whenJust x $ \y -> windows (W.greedyView y . W.shift y)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- A nice little example of extensiblestate
newtype FocusFollow = FocusFollow {getFocusFollow :: Bool } deriving (Typeable,Read,Show)
instance ExtensionClass FocusFollow where
    initialValue = FocusFollow True
    extensionType = PersistentExtension

-- this eventHook is the same as from xmonad for handling crossing events
focusFollow e@(CrossingEvent {ev_window=w, ev_event_type=t})
                | t == enterNotify, ev_mode e == notifyNormal =
        whenX (XS.gets getFocusFollow) (focus w) >> return (All True)
focusFollow _ = return (All True)

toggleFF = XS.modify $ FocusFollow . not . getFocusFollow
--------------------------------------------------------------------------------
