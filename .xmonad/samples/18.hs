-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/doitan%27s_xmonad.hs
#
# .xmonad/xmonad.hs
#
# Requires xmonad, xmonad-contrib and xmobar with xft
#
# trayer, urxvt, libnotify-bin, googlizer
#

import XMonad hiding ( (|||) )
import System.Exit
import System.IO
import Data.Ratio ((%))
import Data.List

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
import XMonad.Actions.Search
import XMonad.Actions.WindowGo
import XMonad.Actions.Promote
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.ConstrainedResize as SQR
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.RotSlaves
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SinkAll
import qualified XMonad.Actions.FlexibleResize as FlexR
import XMonad.Actions.FloatKeys

import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.DwmStyle
import qualified XMonad.Layout.Magnifier as Mag

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Layout

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
import XMonad.Util.Font
import XMonad.Util.XSelection
import XMonad.Util.WorkspaceCompare
import XMonad.Util.WindowProperties
import XMonad.Util.NamedWindows (getName)

myTerminal      = "urxvt"
myBorderWidth   = 1
myModMask       = mod4Mask
myNumlockMask   = mod2Mask

myNormalBorderColor  = "snow4"
myFocusedBorderColor = "#61ce3c"

-- shell prompt theme
mySP = defaultXPConfig
       { font = "xft:DejaVu Sans Mono:pixelsize=14"
       , bgColor           = "#0c1021"
       , fgColor           = "#f8f8f8"
       , fgHLight          = "#f8f8f8"
       , bgHLight          = "steelblue3"
       , borderColor       = "DarkOrange"
       , promptBorderWidth = 1
       , position          = Top
       , height            = 22
       , defaultText       = []
       }

myAutoSP = mySP { autoComplete       = Just 1000 }
myWaitSP = mySP { autoComplete       = Just 1000000 }

delicious = searchEngine "delicious" "http://delicious.com/doitian/"
searchEngineMap method = M.fromList $
    [ ((0, xK_b), method delicious)
    , ((0, xK_d), method dictionary)
    , ((0, xK_g), method google)
    , ((0, xK_w), method wikipedia) ]

xmobarStrip :: String -> String
xmobarStrip = strip [] where
    strip keep x
      | null x                 = keep
      | "<fc="  `isPrefixOf` x = strip keep (drop 1 . dropWhile (/= '>') $ x)
      | "</fc>" `isPrefixOf` x = strip keep (drop 5  x)
      | '<' == head x          = strip (keep ++ "<") (tail x)
      | otherwise              = let (good,x') = span (/= '<') x
                                 in strip (keep ++ good) x'

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                safeSpawn "notify-send" (show name ++ " requests your attention on workspace " ++ index)

myXmobarPP h = xmobarPP
               { ppOutput = hPutStrLn h
               , ppCurrent = xmobarColor "#f8f8f8" "DodgerBlue4" . wrap " " " "
               , ppVisible = xmobarColor "#f8f8f8" "LightSkyBlue4" . wrap " " " "
               , ppUrgent = xmobarColor "#f8f8f8" "red4" . wrap " " " " . xmobarStrip
               , ppLayout = wrap " |" "" . xmobarColor "DarkOrange" "" . wrap " [" "] "
               , ppTitle = xmobarColor "#61ce3c" "" . shorten 50
               , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
               , ppSep = ""
               , ppWsSep = " | "
               }

myTheme :: Theme
myTheme = defaultTheme
          { fontName = "xft:DejaVu Sans YuanTi:pixelsize=14"
          , decoHeight = 22
          , decoWidth = 400
          , activeColor = "grey75"
          , inactiveColor = "grey30"
          , urgentColor = "grey30"
          , activeBorderColor = "#61ce3c"
          , inactiveBorderColor = "grey40"
          , urgentBorderColor = "red"
          , activeTextColor = "black"
          , inactiveTextColor = "grey80"
          , urgentTextColor = "grey80"
          }

myLayoutPrompt = inputPromptWithCompl myAutoSP "Layout"
                 (mkComplFunFromList' ["1.Tall", "2.Wide", "3.2Col", "4.2Row", "5.Tab"]) ?+ \l ->
                     sendMessage $ JumpToLayout $ drop 2 l

-- unused char
-- x, c, y, ', \, d
myKeys =  \conf -> mkKeymap conf $
    [ ("M-S-<Return>", spawn $ XMonad.terminal conf) -- terminal
    , ("M-`", scratchpadSpawnAction conf) -- quake terminal

    -- prompt
    , ("M-p r", shellPrompt mySP) -- shell prompt
    , ("M-p t", prompt (myTerminal ++ " -e") mySP) -- run in term
    , ("M-p g", windowPromptGoto myWaitSP) -- window go prompt
    , ("M-p b", windowPromptBring myWaitSP) -- window bring prompt
    , ("M-p d", AL.launchApp mySP { defaultText = "~" } "thunar" ) -- thunar prompt
    , ("M-g", windowPromptGoto myWaitSP)
    , ("M-b", windowPromptBring myWaitSP)
    , ("M-<Return>", runOrRaise "gmrun" (className =? "Gmrun")) -- gmrun

    , ("M-s", -- search b:delicious d:dictionary g:google w:wikipedia
       (SM.submap $ searchEngineMap $ promptSearch mySP)
       >> raise (className =? "Firefox"))
    , ("M-S-s", spawn "googlizer") -- search selection
    , ("M-C-s", promptSelection "gnome-open") -- search selection

    -- app
    , ("M-o", runOrRaiseNext "firefox" (className =? "Firefox")) -- firefox
    , ("M-S-o", spawn "firefox") -- new firefox
    , ("M-i", runOrRaiseNext "emacs" (className =? "Emacs")) --emacs
    , ("M-u", runOrRaiseNext "urxvt" (className =? "URxvt" <&&> appName /=? "scratchpad")) -- raise next terminal

    , ("M-c p", runOrRaiseNext "gksu synaptic" (className =? "Synaptic")) -- Synaptic
    , ("M-c t", runOrRaiseNext "urxvt -name htop -e htop" (appName =? "htop")) -- Top
    , ("M-c q", runOrRaiseNext "qq" (className =? "Qq")) --QQ
    , ("M-c M-C-S-q", spawn "gksu poweroff") -- PowerOff
    , ("M-c h", spawn "/home/ian/bin/xmonad_key.sh") -- Help
    , ("M-c i", spawn "/home/ian/bin/xp.sh") -- Window Info
    , ("M-c m", runOrRaiseNext "sonata" (className =? "Sonata")) -- Music Player
    , ("M-c x", spawn "xkill") -- Kill X app

    -- client
    , ("M-S-c", kill1) -- kill
    , ("M-C-c", kill) -- kill all
    , ("M-C-S-c", kill) -- kill all
    , ("M-S-<Backspace>", kill1) -- kill
    , ("M-S-=", windows copyToAll) -- copy to all
    , ("M-=",  killAllOtherCopies) -- kill others
    , ("M-C-S-=",  kill) -- kill all

    , ("M-C-<Tab>", rotSlavesUp) -- rotate slaves
    , ("M-<Tab>", windows W.focusDown) -- focus down
    , ("M-S-<Tab>", windows W.focusUp  ) -- focus up
    , ("M-<Page_Down>", windows W.focusDown) -- focus down
    , ("M-<Page_Up>", windows W.focusUp  ) -- focus up
    , ("M-m", windows W.focusMaster  ) -- focus master
    , ("M-z", focusUrgent) -- focus urgent
    , ("M-;", promote) -- promote to master
    , ("M-S-<Page_Down>", windows W.swapDown  ) -- swap down
    , ("M-S-<Page_Up>", windows W.swapUp    ) -- swap up

    , ("M-[", sendMessage Shrink) -- shrink master
    , ("M-]", sendMessage Expand) -- expand master
    , ("M-S-[", sendMessage MirrorShrink) -- shrink window in slave pane
    , ("M-S-]", sendMessage MirrorExpand) -- expand window in slave pane

    , ("M-h", sendMessage $ Go L) -- focus left
    , ("M-j", sendMessage $ Go D) -- focus down
    , ("M-k", sendMessage $ Go U) -- focus up
    , ("M-l", sendMessage $ Go R) -- focus right
    , ("M-S-h", sendMessage $ Swap L) -- swap left
    , ("M-S-j", sendMessage $ Swap D) -- swap down
    , ("M-S-k", sendMessage $ Swap U) -- swap up
    , ("M-S-l", sendMessage $ Swap R) -- swap right
    , ("M-C-h", sendMessage $ Move L) -- move left
    , ("M-C-j", sendMessage $ Move D) -- move down
    , ("M-C-k", sendMessage $ Move U) -- move up
    , ("M-C-l", sendMessage $ Move R) -- move right

    -- float
    , ("M-<L>", withFocused (keysMoveWindow (-20,0))) -- move float left
    , ("M-<R>", withFocused (keysMoveWindow (20,0))) -- move float right
    , ("M-<U>", withFocused (keysMoveWindow (0,-20))) -- move float up
    , ("M-<D>", withFocused (keysMoveWindow (0,20))) -- move float down
    , ("M-S-<L>", withFocused (keysResizeWindow (-20,0) (0,0))) --shrink float at right
    , ("M-S-<R>", withFocused (keysResizeWindow (20,0) (0,0))) --expand float at right
    , ("M-S-<D>", withFocused (keysResizeWindow (0,20) (0,0))) --expand float at bottom
    , ("M-S-<U>", withFocused (keysResizeWindow (0,-20) (0,0))) --shrink float at bottom
    , ("M-C-<L>", withFocused (keysResizeWindow (20,0) (1,0))) --expand float at left
    , ("M-C-<R>", withFocused (keysResizeWindow (-20,0) (1,0))) --shrink float at left
    , ("M-C-<U>", withFocused (keysResizeWindow (0,20) (0,1))) --expand float at top
    , ("M-C-<D>", withFocused (keysResizeWindow (0,-20) (0,1))) --shrink float at top

    -- layout
    , ("M-\\", sendMessage NextLayout) -- toggle layouts
    , ("M-S-\\", myLayoutPrompt) -- layout prompt
    , ("M-C-\\", setLayout $ XMonad.layoutHook conf) -- reset layout
    , ("M-f", sendMessage (Toggle "Full")) -- toggle Full
    , ("M-<Escape>", sendMessage ToggleStruts) -- toggle panel
    , ("M-t", withFocused $ windows . W.sink) -- sink focused window
    , ("M-S-t", sinkAll) -- sink all windows
    , ("M-C-[", sendMessage (IncMasterN 1)) -- increase master windows number
    , ("M-C-]", sendMessage (IncMasterN (-1))) --decrease master windows number

    -- system
    , ("M-C-S-q", io (exitWith ExitSuccess)) -- exit
    , ("M-S-q",  broadcastMessage ReleaseResources >> restart "xmonad" True) -- restart
    , ("M-q", refresh) -- refresh

    -- cycle through workspaces
    , ("M-n", selectWorkspace mySP) -- workspace prompt
    , ("M-S-n", withWorkspace mySP (windows . W.shift)) -- workspace shift prompt
    , ("M-C-n", withWorkspace mySP (windows . copy)) -- workspace copy prompt
    , ("M-C-S-n", renameWorkspace mySP) -- rename workspace
    , ("M-C-S-<Backspace>", removeWorkspace) -- delete empty workspace
    , ("M-/", toggleWSNoSP) -- toggle recently visited workspaces

    -- misc
    ]
    ++
    -- "M-[1..9,0,-]" -- Switch to workspace N
    -- "M-S-[1..9,0,-]" -- Move client to workspace N
    -- "M-C-[1..9,0,-]" -- Copy client to workspace N
    [("M-" ++ m ++ [k], windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] ++ ['0', '-'])
        , (f, m) <- [ (W.greedyView, "")
                    , (W.shift, "S-")
                    , (copy, "C-")
                    ]
    ]
    ++
    -- "M-C-S-[1..9,0,-]" -- Move client to workspace N and follow
    [("M-C-S-" ++ [k], (windows $ W.shift i) >> (windows $ W.greedyView i))
        | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'] ++ ['0', '-'])
    ]
    ++
    -- "M-{w,e,r}" -- Switch to physical/Xinerama screens 1, 2, or 3
    -- "M-S-{w,e,r}" -- Move client to screen 1, 2, or 3
    --
    [("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

    ++
    [ ("M-.", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1) -- go to next workspace
    , ("M-,", windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1) -- go to prev workspace
    , ("M-S-.", windows . W.shift =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1) -- shift to next workspace
    , ("M-S-,", windows . W.shift =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1) -- shift to prev workspace
    -- move window to and focus HiddenNonEmpty wss except scratchpad
    , ("M-C-S-.", shiftAndView' Next) -- shift to next workspace and follow
    , ("M-C-S-,", shiftAndView' Prev) -- shift to prev workspace and follow
    ]
    where 
      getSortByIndexNoSP =
          fmap (.scratchpadFilterOutWorkspace) getSortByIndex
      shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
                          >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
      toggleWSNoSP = windows $ W.view =<< W.tag . head . scratchpadFilterOutWorkspace . W.hidden

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3),               (\w -> focus w >> FlexR.mouseResizeWindow w))
    , ((modMask .|. shiftMask, button3), (\w -> focus w >> SQR.mouseResizeWindow w True ))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:
myLayout = ewmhDesktopsLayout
           $ avoidStruts
           $ configurableNavigation (navigateColor "snow1")
           $ toggleLayouts (noBorders Full)
           $ smartBorders
           $ onWorkspace "9.im" pidginLayout
           $ onWorkspace "0.qq" qqLayout
           $ onWorkspace "-" mgrid
           $ basicLayout
  where
    basicLayout  = tall ||| wide ||| twoCol ||| twoRow ||| tabs
    rtiled       = layoutHints $ ResizableTall nmaster delta ratio []
    tall         = named "Tall" $ deco $ rtiled
    wide         = named "Wide" $ deco $ Mirror rtiled
    twoPane      = layoutHints $ TwoPane delta ratio
    twoCol       = named "2Col" $ deco $ twoPane
    twoRow       = named "2Row" $ deco $ Mirror twoPane
    tabs         = named "Tab" $ layoutHints $ tabbed shrinkText myTheme
    deco         = dwmStyle shrinkText myTheme
    nmaster      = 1
    ratio        = 1/2
    delta        = 3/100
    pidginLayout = named "IM" $ (withIM (1%7) pidginRoster grid)
    qqLayout     = named "IM" $ (withIM (1%7) qqRoster grid)
    grid         = layoutHints Grid
    mgrid        = named "Grid" $ Mag.magnifiercz 1.15 $ grid
    pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
    qqRoster     = And (ClassName "Qq") (Title "QQ")

myWorkspaces    = ["1.sys","2.www","3.emacs","4.doc","5","6","7","8","9.im","0.qq","-"]
                   
myAdditionalManageHook = composeOne $
    [ transience ]
    ++
    [ className =? c                -?> doIgnore | c <- ignoreC ]
    ++
    [ className =? c                -?> doFloat | c <- floatC ]
    ++
    [ className =? c                -?> doCenterFloat | c <- centerFloatC ]
    ++
    [ className =? "Firefox" <&&> resource =? r -?> doCenterFloat
      | r <- floatFF ]
    ++
    -- auto shift
    [ className =? c         -?> doShift t
      | (c, t) <- [ ("Pidgin", "9.im")
                  , ("Qq", "0.qq")
                  , ("Firefox", "2.www")
                  , ("Emacs", "3.emacs")
                  ]
    ]
    ++
    -- auto copy
    [ className =? c         -?> (ask >>= doF .  \w -> (copyWindow w t))
      | (c, t) <- [ ("Evince", "4.doc")
                  , ("Xpdf", "4.doc")
                  ]
    ]
    where
      unFloat = ask >>= doF . W.sink
      floatFF = [ "DTA", "Manager", "Extension", "Download",
                  "Dialog", "Browser", "Toplevel" ]
      floatC  = [ "Zenity", "Twhirl", "Gcalctool", "Airappinstaller", "Qt-dotnet.dll" ]
      ignoreC = [ "Do", "trayer" ]
      centerFloatC = [ "Gcolor2", "Sonata", "Stardict", "Update-manager" ]

myManageHook = manageDocks
               <+> (doF avoidMaster)
               <+> myAdditionalManageHook
               <+> composeAll [(isFullscreen --> doFullFloat)]
               <+> scratchpadManageHook (W.RationalRect 0 0 1 0.3)
               <+> manageHook defaultConfig

avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    otherwise           -> c

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

main = do
  xmobar <- spawnPipe "/usr/bin/xmobar /home/ian/.xmobarrc"
  xmonad $ withUrgencyHook LibNotifyUrgencyHook defaultConfig
             { terminal           = myTerminal
             , focusFollowsMouse  = myFocusFollowsMouse
             , borderWidth        = myBorderWidth
             , modMask            = myModMask
             , numlockMask        = myNumlockMask
             , workspaces         = myWorkspaces
             , normalBorderColor  = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor

             -- key bindings
             , keys               = myKeys
             , mouseBindings      = myMouseBindings

             -- hooks, layouts
             , layoutHook         = myLayout
             , manageHook         = myManageHook
             , logHook            = ewmhDesktopsLogHook >> (dynamicLogWithPP $ myXmobarPP xmobar)
                                    >> updatePointer Nearest
             , startupHook        = myStartupHook
             }
             `additionalKeys`
             [ ((0, 0x1008ff18), AL.launchApp mySP { defaultText = "~" } "thunar" )
             , ((0, 0x1008ff26), spawn "mpc prev")
             , ((0, 0x1008ff27), spawn "mpc next")
             ]
