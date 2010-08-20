-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Erthad%27s_xmonad.hs
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

{-# OPTIONS_GHC -fglasgow-exts #-} -- required for XMonad.Layout.MultiToggle

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.RotSlaves
import XMonad.Actions.Search
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog as Log
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Workspace
import XMonad.Util.Dzen as Dzen
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Layout.Accordion
import XMonad.Layout.Combo
import XMonad.Layout.DwmStyle
import XMonad.Layout.IM
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation

import Codec.Binary.UTF8.String
import Control.Monad
import Data.Ratio ((%))
import System.Exit
import System.IO (hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "xterm -bg black -fg '#CFDBFF' -fn '-*-terminus-medium-*-*-*-20-*-*-*-*-*-iso10646-1' +sb -bdc"

myBorderWidth   = 1
myModMask       = mod4Mask
myNumlockMask   = mod2Mask

myWorkspaces    = ["~"] ++ map show [1..2] ++ ["web","IM","mail","6","7","8","mpc","0","rdesktop"]
myDefaultGaps   = [(0,0,0,0)]

myNormalBorderColor  = "#444488"
myFocusedBorderColor = "#ee9999"

myBgColor= "#001070"
myFgColor = "#bbbbdd"
myBgHLight= "#4444aa"
myFgHLight= "#ddddff"

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
              { font        = "xft:Terminus:pixelsize=16"
	      , bgColor     = myBgColor
	      , fgColor     = myFgColor
	      , bgHLight    = myBgHLight
	      , fgHLight    = myFgHLight
              , borderColor = myNormalBorderColor
              }

-- Mutimedia keys bindings
multKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $ [
      ((0, 0x1008ff14), spawn "mpc --no-status toggle")
    , ((0, 0x1008ff15), spawn "mpc --no-status stop")
    , ((0, 0x1008ff16), spawn "mpc --no-status prev")
    , ((0, 0x1008ff17), spawn "mpc --no-status next")
    , ((0, 0x1008ff11), spawn "mpcvolume -")
    , ((0, 0x1008ff13), spawn "mpcvolume +")

    , ((0, 0x1008ff18), runOrRaise "firefox" (className =? "Firefox-bin"))
    , ((0, 0x1008ff19), runOrRaise "claws-mail" (className =? "Claws-mail"))
    , ((0, 0x1008ff30), runOrRaise "kvirc" (className =? "Kvirc"))

    , ((0, 0x1008ff10), spawn "/usr/bin/xscreensaver-command -lock")
    , ((0, xK_Print), unsafeSpawn "import -w root png:$HOME/screenshot--`date +%F--%T`.png")
    ]

myKeys = \conf -> mkKeymap conf $ [

      ("M-S-<Return>", spawn $ XMonad.terminal conf)	-- terminal

    , ("M-S-c", kill)
    , ("M-r", refresh)
    , ("M-b", sendMessage ToggleStruts)

    , ("M-S-C-<Pause>", io (exitWith ExitSuccess))
    , ("M-q", restart "xmonad" True)

-- Propmpts here
    , ("M-p p", runOrRaisePrompt myXPConfig)
    , ("M-p M-p", runOrRaisePrompt myXPConfig)
    , ("M-p t", prompt (myTerminal ++ " -e") myXPConfig)
    , ("M-p M-t", prompt (myTerminal ++ " -e") myXPConfig)
    , ("M-p s", sshPrompt myXPConfig)
    , ("M-p M-s", sshPrompt myXPConfig)
    , ("M-p w", workspacePrompt myXPConfig (windows . W.view))
    , ("M-p S-w", workspacePrompt myXPConfig (windows . W.shift))
    , ("M-p M-w", workspacePrompt myXPConfig (windows . W.view))
    , ("M-p M-S-w", workspacePrompt myXPConfig (windows . W.shift))
    , ("M-S-b", selectSearch bugzilla)
    , ("M-p M-r", prompt ("rdesktop -k en-us") myXPConfig)

-- Workspaces
    , ("M-z", viewEmptyWorkspace) 
    , ("M-<L>", prevWS)
    , ("M-<R>", nextWS)

    , ("M-<Space>", sendMessage NextLayout)
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)

--- Window management
    , ("M-h", sendMessage $ Go L)
    , ("M-S-h", sendMessage $ Swap L)
    , ("M-j", sendMessage $ Go D)
    , ("M-S-j", sendMessage $ Swap D)
    , ("M-k", sendMessage $ Go U)
    , ("M-S-k", sendMessage $ Swap U)
    , ("M-l", sendMessage $ Go R)
    , ("M-S-l", sendMessage $ Swap R)

    , ("M-<Tab>", windows W.focusDown)
    , ("M-S-<Tab>", windows W.focusUp)

    , ("M-<Return>", windows W.focusMaster)
    , ("M-m", dwmpromote)

    , ("M-y", rotSlavesDown)
    , ("M-S-y", rotAllDown)
    , ("M-o", rotSlavesUp)
    , ("M-S-o", rotAllUp)

    , ("M-S-u", sendMessage Shrink)
    , ("M-S-i", sendMessage Expand)
    , ("M-u", sendMessage MirrorExpand)
    , ("M-i", sendMessage MirrorShrink)

    , ("M-v", withFocused $ sendMessage . maximizeRestore )
    , ("M-s", sendMessage $ Toggle ACCORDION)
    , ("M-S-s", sendMessage $ Toggle FULL)
    , ("M-t", withFocused $ windows . W.sink)	-- unfloat

    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    ]

    ++

    [ (m ++ i, windows $ f j)
                    | (i, j) <- zip (["`"] ++ map show [1..9] ++ ["0","C-r"] ) (XMonad.workspaces conf)
                    , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
--                    , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
    ]

--- xinerama not used
--    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
--    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
--        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

bugzilla = searchEngine "bugzilla" "https://bugzilla.altlinux.org/show_bug.cgi?id="

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
--    , ((modMask, button4), prevWS)
--    , ((modMask, button5), nextWS)
    ]

data ACCORDION = ACCORDION deriving (Read, Show, Eq, Typeable)
instance Transformer ACCORDION Window where
      transform _ x k = k Accordion

data FULL = FULL deriving (Read, Show, Eq, Typeable)
instance Transformer FULL Window where
      transform _ x k = k Full

data NOBORDERS = NOBORDERS deriving (Read, Show, Eq, Typeable)
instance Transformer NOBORDERS Window where
      transform _ x k = k (noBorders x)

myLayout = ewmhDesktopsLayout . dwmStyle shrinkText myTheme . windowNavigation . avoidStruts . maximize . mkToggle (single ACCORDION) . mkToggle (NOBORDERS ?? FULL ?? EOT) $ onWorkspace "IM" tiledIM (Mirror tiled) ||| onWorkspace "IM" (Mirror tiled) tiledIM
  where
     tiled   = ResizableTall 1 0.03 0.6180 []
     tiledIM   = ResizableTall 1 0.03 0.85 []

myTheme = defaultTheme {
            activeColor = myBgHLight
          , activeTextColor = myFgHLight
	  , inactiveColor = myBgColor
	  , inactiveTextColor = myFgColor
          }

myManageHook = composeAll
     [
       moveToC "Firefox-bin" "web"
     , floatT "Save a Bookmark"
     , moveToC "Claws-mail" "mail"
     , moveToC "rdesktop" "rdesktop"
     , moveToC "Kvirc" "IM"
     , moveToC "mainwnd" "IM"
     , floatC "Xmessage"
     , floatC "container"
     , floatC "history"
     , floatT $ decode [0xd0, 0x9f, 0xd0, 0xbe, 0xd0, 0xbc, 0xd0, 0xb5, 0xd1, 0x82, 0xd0, 0xb8, 0xd1, 0x82, 0xd1, 0x8c, 0x20, 0xd0, 0xba, 0xd0, 0xb0, 0xd0, 0xba, 0x20, 0xd0, 0xbf, 0xd1, 0x80, 0xd0, 0xbe, 0xd1, 0x87, 0xd0, 0xb8, 0xd1, 0x82, 0xd0, 0xb0, 0xd0, 0xbd, 0xd0, 0xbd, 0xd0, 0xbe, 0xd0, 0xb5]
     ] <+> manageDocks
     where
       moveToC c w = className =? c --> doF (W.shift w)
       moveToT t w = title =? t --> doF (W.shift w)
       floatC c = className =? c --> doFloat
       floatT t = title =? t --> doFloat

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
myStartupHook = do
  spawn "xsetroot -cursor_name left_ptr -solid '#001040'"
--return ()


--myLogHook = return ()


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  dzen2 <- spawnPipe ( "killall rundzen; rundzen --" ++ (join $ map (wrap "-" "-") myWorkspaces ))

  Log.dzen $ \ defaultConfig -> xmonad $

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
--
    defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
--        defaultGaps        = myDefaultGaps,

      -- key bindings
        keys               = \c -> myKeys c `M.union` multKeys c,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = do
	                       ewmhDesktopsLogHook
			       dynamicLogWithPP dzenPP {
							  ppCurrent = dzenColor myFgHLight myBgHLight . wrap "-" "-"
							  , ppTitle = (>> "")
							  , ppLayout = (>> "")
							  , ppHidden = dzenColor myFgColor myBgColor . wrap " " " "
							  , ppSep = " | "
--							  , ppExtras = [ logCmd "gcpubar -c 1" ]
	                                                  , ppOutput = hPutStrLn dzen2 },
        startupHook        = myStartupHook
    }

