-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/hgabreu%27s_xmonad.hs
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, ParallelListComp, TypeSynonymInstances #-}

import Control.Arrow (first)
import Control.Monad (filterM,liftM,join)
import Data.Char (toLower)
import Data.List
import Data.Maybe (fromJust)
import Data.IORef
import System.Posix.Files (isDirectory,getFileStatus)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.BoringWindows hiding (Merge)
import XMonad.Layout.Column
import XMonad.Layout.ComboP
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell hiding (getShellCompl)
import XMonad.Util.EZConfig
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.WorkspaceCompare
import qualified Data.Set as S
import qualified Data.Map as M
import qualified XMonad.Actions.ConstrainedResize as Sqr
import qualified XMonad.StackSet as W

main = do
    toggleFadeSet <- newIORef S.empty --to toggle windows fade with keybind
    xmonad $ myConfig toggleFadeSet

myConfig toggleFadeSet = defaultConfig
    { layoutHook         = myLayout
    , manageHook         = manageDocks <+> myManageHook <+> manageSpawn
    , startupHook        = ewmhDesktopsStartup >> setWMName "LG3D"
    , handleEventHook    = ewmhDesktopsEventHook
    , logHook            = myLogHook toggleFadeSet >> ewmhDesktopsLogHook
    , borderWidth        = 2 --border of floating windows, since I user noBorders for all tiled
    , normalBorderColor  = "gray"
    , focusedBorderColor = "black"
    , terminal           = "urxvtc"
    , modMask            = mod4Mask
    , mouseBindings      = myMouse
    , workspaces         = myWorkspaces
    } `removeKeysP`
    [ "M-q", "M-S-q" --, "M4-z", "M4-x", "M4-c", "M4-v", "M4-b"
    ] `additionalKeysP` myKeys toggleFadeSet

myWorkspaces = ["web","dev","ter","im","5","6"]
stickyWS = ["web","dev","ter","im"]
jdevClass = "oracle-ide-boot-Launcher" --jdeveloper
browser="Chromium"

myLayout = fixFocus $ avoidStruts $ noBorders $ toggleLayouts Full $ configurableNavigation noNavigateBorders $ boringWindows $
    onWorkspace "im" im $ onWorkspaces ["dev","6"] l2 $ l1 where
--MouseResizableTile and subLayouts don't play well together, mine has a minor edit
--I commented at mrt line 150: {-draggerWrs ++-}
        l1 = subThis (Simplest ||| mrt 1 0.5) $ mrt 2 0.6 ||| simpleTabbed
        l2 = subThis (mrt 1 0.5 ||| Simplest) $ simpleTabbed ||| mrt 1 0.5
        im  = combineTwoP (tall 0.13) (Mirror $ tall 0.55) l1 (pidgin `Or` skype)
        mrt n r = mouseResizableTile{ nmaster=n, masterFrac=r, slaveFrac=r, fracIncrement=0.02, draggerType=(FixedDragger 3 3) }
        subThis li lo = addTabs shrinkText defaultTheme $ subLayout [] li lo
        tall r = noMessages $ Tall 1 0.02 r
        pidgin = ClassName "Pidgin" `And` Role "buddy_list"
        skype = Title "<myuser> - Skype™ (Beta)" `Or` Title "Skype™ 2.1 (Beta) for Linux"

q ~? x  = fmap (x `isInfixOf`) q

myManageHook = (composeAll . concat $ --shifting actions
    [ [ title     =? x --> doCenterFloatToAll | x <- important ]
    , [ className =? x --> doShift "web"      | x <- cShiftWeb ]
    , [ className =? x --> doShiftAndGo "dev" | x <- cShiftDev ]
    , [ className =? x --> doShift "im"       | x <- cShiftIm  ]
    , [ className =? x --> doShift "6"        | x <- cShift6   ]
    ]) <+> (composeOne . concat $     --floating actions
    [ [ className =? x -?> doCenterFloat'     | x <- cCenterF  ]
    , [ title     =? x -?> doCenterFloat'     | x <- tCenterF  ]
    , [ title     ~? x -?> doCenterFloat'     | x <- ptCenterF ]
    , [ className ~? x -?> doCenterFloat'     | x <- pcFloat   ]
    , [ className =? x -?> doFloat'           | x <- cFloat    ]
    , [ title     =? x -?> doFloat'           | x <- tFloat    ]
    , [ className =? x -?> doMaster           | x <- masters   ]
    , [ className =? "Pidgin" <&&> role =? "buddy_list" -?> doMaster ]
    , [ className =? "Orage" -?> doFloatAt' (46/1680) (1-176/1050) ]
    , [ isFullscreen   -?> doFullFloat'   ]
    , [ isDialog       -?> doCenterFloat' ]
    , [ className =? "" <&&> title =? "" -?> doFullFloat' ] --slim preview
    , [ className =? "Skype" <&&> title =? "Options" -?> doCenterFloat' ]
    , [ className =? "Skype" <&&> title ~? "Call with " -?> doSideFloat' CE ]
    , [ appName   =? "floatingTerminal" -?> doRectFloat' (W.RationalRect 0.65 0.65 0.3 0.3) ] --x y w h
    , [ className =? "Xfce4-notifyd" -?> doCopyToAll ]
    --, [ className =? "URxvt" -?> queryMerge (className =? "URxvt") ]
    , [ return True -?> doF W.swapDown ] --if the window is not floating => swapDown to prevent changing the master
    ]) where
        cShiftWeb = [browser,"Namoroka"]
        cShiftDev = [jdevClass]
        cShiftIm  = ["Pidgin","Skype"]
        cShift6   = ["VirtualBox"]
        important = ["New Pounces","XMPP Message Error"]
        cCenterF  = ["MPlayer","xine","Xitk","Speedcrunch","com-sun-javaws-Main","Blueman-manager"
                    ,"Pavucontrol","Xmessage","Wicd-client.py","Xdialog"]
        tCenterF  = ["Event Tester","Plugins","Add-ons","Archive manager","Chromium Options"]
        ptCenterF = ["About","Buddy Pounce"]
        pcFloat   = ["Xfce4-","br-com-atenacs-"]
        cFloat    = ["Xfrun4","ClienteApp","principal-Cliente","Gimp","Gimp-2.6","Mousepad","Wine"]
        tFloat    = ["glxgears","Java-Test"]
        masters   = [browser,jdevClass]
        role = stringProperty "WM_WINDOW_ROLE"
        doMaster = doF W.shiftMaster --append this to all floats so new windows always go on top, regardless of the current focus
        doFloat' = doFloat <+> doMaster
        doCenterFloat' = doCenterFloat <+> doMaster
        doFloatAt' x y = doFloatAt x y <+> doMaster
        doSideFloat' p = doSideFloat p <+> doMaster
        doRectFloat' r = doRectFloat r <+> doMaster
        doFullFloat' = doFullFloat <+> doMaster
        doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
        doCopyToAll = ask >>= doF . \w -> (\ws -> foldr($) ws (map (copyWindow w) myWorkspaces))
        doCenterFloatToAll = doCopyToAll <+> doCenterFloat'

queryMerge :: Query Bool -> ManageHook
queryMerge pGrp = do
    w <- ask
    aws <- liftX $ filterM (runQuery pGrp) =<< gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
    -- now add the win ourselves:
    -- liftX $ modify (\ws -> ws { windowset = W.insertUp w (windowset ws) })
    liftX $ windows (W.insertUp w)
    mapM_ (liftX . sendMessage . Merge w) aws
    idHook

myLogHook toggleFadeSet = fadeOutLogHook $ fadeIf (testCondition toggleFadeSet) 0.7
doNotFadeOutWindows = title ~? "Call with " <||> className =? "xine" <||> className =? "MPlayer"

testCondition :: IORef (S.Set Window) -> Query Bool
testCondition floats =
    liftM not doNotFadeOutWindows <&&> isUnfocused
    <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)

toggleFadeOut :: Window -> S.Set Window -> S.Set Window
toggleFadeOut w s | w `S.member` s = S.delete w s
                  | otherwise = S.insert w s

myKeys toggleFadeSet = (
    [ ("C-<Esc>", spawn "xfce4-session-logout")
    , ("M-<Esc>", spawn "xscreensaver-command -lock")
    , ("M1-<F1>", spawn "xfce4-popup-menu")
    , ("M1-<F2>", spawn "xfrun4")
    , ("M-S-<Backspace>", spawn "urxvtc -name floatingTerminal")
    , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
    --, ("M-p", spawn "dmenu_run -i") --dmenu case insensitive
    , ("M-p", myShellPromptHere defaultXPConfig{position=Top} )
    , ("M-S-p", spawn "xfce4-appfinder")
    -- winamp and amarok like keybinds for exaile
    , ("M4-z", spawn "exaile -p") --play previous track
    , ("M4-x", spawn "exaile -a") --play currently selected or queued song
    , ("M4-c", spawn "exaile -t") --pause or resume playback
    , ("M4-v", spawn "exaile --stop-after-current") -- :)
    , ("M4-b", spawn "exaile -n") --play next track
    , ("M-<Return>", windows $ W.shiftMaster)
    , ("M-/", kill)
    , ("M-S-/", withFocused $ \w -> spawn ("xkill -id " ++ show w)) --xkill focused
    , ("M-f", sendMessage ToggleStruts >> sendMessage ToggleLayout) --toggle fullscreen
    , ("M-S-f", withFocused $ io . modifyIORef toggleFadeSet . toggleFadeOut)
    , ("M-a", windows $ \ws -> foldr copy ws stickyWS) --doCopyToAll with exceptions
    , ("M-S-a", killAllOtherCopies)                    --undo copy to all
    , ("M-'", toggleWS )
    , ("M-\\", moveTo Next EmptyWS)
    , ("M-S-\\",  --moveTo Next EmptyWS with window
        do t <- findWorkspace getSortByIndex Next EmptyWS 1
           windows . W.shift $ t
           windows . W.view $ t)
    , ("M-d", spawn "thunar /mnt/dados")
    , ("M-s", spawn "speedcrunch")
    , ("<Print>", spawn "screenshot.sh full")
    , ("M1-<Print>", spawn "screenshot.sh")
    , ("M-q", spawn "xrandr -o left")
    , ("M-w", spawn "xrandr -o normal")
    , ("M-e", spawn "xfce4-panel -c")
    , ("M-M1-m", withFocused $ \w -> windows (W.sink w) >> sendMessage (mergeDir W.focusDown' w))
    , ("M-M1-n", withFocused $ sendMessage . UnMerge)
    , ("M-M1-<Space>", toSubl NextLayout)
    , ("M-<Tab>", focusDown)
    , ("M-S-<Tab>", focusUp)
    , ("M-m", focusMaster)
    , ("M-M1-<Tab>", onGroup W.focusDown')
    , ("M-S-M1-<Tab>", onGroup W.focusUp')
    , ("M-M1-,", toSubl (IncMasterN 1))
    , ("M-M1-.", toSubl (IncMasterN (-1)))
    ] ++ (map (first ("M-" ++)) $
        [ (m ++ k, sendMessage $ c d)  --window navigation
        | (m, c) <- zip ["", "S-", "M1-"] [Go, Swap, pullGroup]
        , (d, k) <- zip (cycle [L, R, U, D]) dirKeys
        ] ++ --window resize
        [ ("C-" ++ k, sendMessage' c) | (k, c) <- zip dirKeys $ cycle resizeMsgs
        ] ++ --resize sublayout
        [ ("C-M1-" ++ k, toSubl' c) | (k, c) <- zip dirKeys $ cycle resizeMsgs
        ] ++ --workspaces navigation
        zip (map concat $ sequence [["","S-","C-"],["o","u"]])
            [nextWS,prevWS,shiftToNext,shiftToPrev,shiftToNext>>nextWS,shiftToPrev>>prevWS]
        ++ --workspaces navigation with keypad
        [ (m ++ "<KP_" ++ k ++ ">", windows $ f i)
        | (i, k) <- zip myWorkspaces ["End","Down","Next","Left","Begin","Right","Home","Up","Prior"]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "S-"), (\w -> W.greedyView w . W.shift w, "C-")]
        ] ++ --add shiftAndGo to default navigation
        [ ("C-" ++ k, windows $ W.greedyView w . W.shift w)
        | (w, k) <- zip myWorkspaces $ map show [1..9]
        ]
    )) where
        dirKeys = ["<L>","<R>","<U>","<D>","j","l","i","k"]
        resizeMsgs = [Left Shrink,Left Expand,Right ShrinkSlave,Right ExpandSlave]

myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> Sqr.mouseResizeWindow w False >> windows W.shiftMaster)) --it's actually my right button, I've switched it with the middle one
    , ((modm, button3), (\w -> windows $ W.shiftMaster . W.focusUp . W.swapDown)) --try to bring under-floating up; works fine with two floating windows
    , ((modm, button4), (\_ -> windows W.focusUp))
    , ((modm, button5), (\_ -> windows W.focusDown))
    , ((modm .|. shiftMask, button2), (\w -> focus w >> Sqr.mouseResizeWindow w True >> windows W.shiftMaster))
    , ((modm .|. shiftMask, button4), (\_ -> windows W.swapUp))
    , ((modm .|. shiftMask, button5), (\_ -> windows W.swapDown))
    , ((modm .|. controlMask, button4), (\_ -> prevWS))
    , ((modm .|. controlMask, button5), (\_ -> nextWS))
    ]

sendMessage' :: (Either Resize MRTMessage) -> X ()
sendMessage' (Left r) = sendMessage r
sendMessage' (Right mr) = sendMessage mr

toSubl' :: (Either Resize MRTMessage) -> X ()
toSubl' (Left r) = toSubl r
toSubl' (Right mr) = toSubl mr

-- suppress messages to layout
data DummyMessage = DummyMessage deriving (Show, Read, Typeable)
instance Message DummyMessage

data NoMessages a = NoMessages deriving (Show, Read)
instance LayoutModifier NoMessages a where
    handleMessOrMaybeModifyIt _ _ = return . Just . Right . SomeMessage $ DummyMessage

noMessages :: l a -> ModifiedLayout NoMessages l a
noMessages = ModifiedLayout NoMessages

-- workaround of xmonad issue 4
data FixFocus a = FixFocus (Maybe a) deriving (Read, Show)
instance LayoutModifier FixFocus Window where
    modifyLayout (FixFocus mlf) ws@(W.Workspace id lay Nothing) r = runLayout ws r
    modifyLayout (FixFocus Nothing) ws r = runLayout ws r
    modifyLayout (FixFocus (Just lf)) (W.Workspace id lay (Just st)) r = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        is_rf_floating <- maybe (return False) (\rf -> withWindowSet $ return . M.member rf . W.floating) mreal_f -- real focused window is floating?
        let new_stack_f = if is_rf_floating then lf else stack_f --if yes: replace stack's focus with our last saved focus
        let new_st' = until (\s -> new_stack_f == W.focus s) W.focusUp' st -- new stack with focused new_stack_f
        let new_st = if (new_stack_f `elem` (W.integrate st)) then new_st' else st -- use it only when it's possible to
        runLayout (W.Workspace id lay (Just new_st)) r

    redoLayout (FixFocus mlf) r Nothing wrs = return (wrs, Just $ FixFocus mlf)
    redoLayout (FixFocus mlf) r (Just st) wrs = do
        let stack_f = W.focus st  -- get current stack's focus
        mst <- gets (W.stack . W.workspace . W.current . windowset)
        let mreal_f = maybe Nothing (Just . W.focus) mst -- get Maybe current real focus
        let crf_in_stack = maybe False ((flip elem) (W.integrate st)) mreal_f -- current real focus belongs to stack?
        let new_saved_f = if crf_in_stack then fromJust mreal_f else stack_f -- if yes: replace saved focus
        return (wrs, Just $ FixFocus $ Just new_saved_f)

fixFocus :: LayoutClass l a => l a -> ModifiedLayout FixFocus l a
fixFocus = ModifiedLayout $ FixFocus Nothing

--my case insensitive shell prompt
myShellPromptHere :: XPConfig -> X ()
myShellPromptHere c = do
    cmds <- io $ getCommands
    mkXPrompt Shell c (getShellCompl cmds) spawnHere

getShellCompl :: [String] -> String -> IO [String]
getShellCompl cmds s | s == "" || last s == ' ' = return []
                     | otherwise                = do
    f     <- fmap lines $ runProcessWithInput "bash" [] ("bind 'set completion-ignore-case on'; compgen -A file " ++ encodeOutput s ++ "\n")
    files <- case f of
               [x] -> do fs <- getFileStatus x
                         if isDirectory fs then return [x ++ "/"]
                                           else return [x]
               _   -> return f
    return . map decodeInput . uniqSort $ files ++ commandCompletionFunction cmds s

commandCompletionFunction :: [String] -> String -> [String]
commandCompletionFunction cmds str | '/' `elem` str = []
                                   | otherwise = filter ((\x y -> map toLower x `isPrefixOf` map toLower y) str) cmds

