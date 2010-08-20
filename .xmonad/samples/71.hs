-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/iderrick_xmonad.hs
--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import System.Exit
import IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.Combo

import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Layout.WindowNavigation

import XMonad.Util.Run

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ServerMode

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtc"
myNavigator     = "firefox"

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod3Mask

-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = map show [1..12]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask              , xK_c     ), sendMessage $ Toggle FULL)
    , ((modMask              , xK_v     ), sendMessage $ Toggle MIRROR)
    , ((modMask              , xK_BackSpace), withFocused (sendMessage . maximizeRestore))

    , ((modMask              , xK_F1    ), spawn $ (XMonad.terminal conf) ++ " -name mutt -e mutt")
    , ((modMask              , xK_F2    ), spawn $ XMonad.terminal conf)
    , ((modMask              , xK_F3    ), spawn $ (XMonad.terminal conf) ++ " -name irssi -e irssi")
    , ((modMask              , xK_F4    ), spawn myNavigator)
    , ((modMask              , xK_F5    ), spawn "emacs")

    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((mod1Mask,              xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modMask .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
    , ((modMask .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
    , ((modMask .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
    , ((modMask .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_semicolon), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    , ((modMask              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask , k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [
                     xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe,
                     xK_parenleft, xK_minus, xK_egrave, xK_underscore, xK_ccedilla,
                     xK_agrave, xK_parenright, xK_equal
                    ]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

myLayout =   avoidStruts
           . smartBorders
           . mkToggle (NOBORDERS ?? FULL ?? EOT)
           . mkToggle (single MIRROR)
           $  tiled
           -- ||| Mirror (TwoPane delta (1/2))
           ||| noBorders Full
--           ||| tab
           ||| latex
    where
      -- default tiling algorithm partitions the screen into two panes
      tiled = maximize (Tall nmaster delta ratio)

--      tab = tabbed shrinkText myTabConfig

      latex = windowNavigation (
                                combineTwo
                                (TwoPane delta 0.45)
                                (Full)
                                (combineTwo
                                 (Mirror (TwoPane delta 0.85))
                                 (Full)
                                 (Full)
                                )
                               )

      -- The default number of windows in the master pane
      nmaster = 1

      -- Default proportion of screen occupied by master pane
      ratio   = 1/2

      -- Percent of screen to increment by when resizing panes
      delta   = 3/100

      myTabConfig = defaultTheme { inactiveBorderColor = "#BFBFBF"
                                 , activeTextColor = "#FFFFFF"}

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageDocks <+> composeAll
--    [ className =? "MPlayer"        --> doFloat
    [ className =? "Gimp"           --> doFloat
    , className =? "Soleil"         --> doFloat
    , className =? "bla"            --> doFloat
    , className =? "xmlimg.opt"     --> doFloat
    , className =? "freetennis"     --> doFloat
    , className =? "xjump"          --> doFloat
    , className =? "Tilda"          --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Emacs"          --> doF (W.shift "2")
    , title     =? "mutt"           --> doF (W.shift "3")
    , title     =? "irssi"          --> doF (W.shift "3")
    , className =? "Galeon"         --> doF (W.shift "4")
    , className =? "Iceweasel"      --> doF (W.shift "4")
    , className =? "Sunbird-bin"    --> doF (W.shift "5")
    , className =? "Liferea-bin"    --> doF (W.shift "5")
    ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--

--
-- voir après

------------------------------------------------------------------------
--commands :: X [(String, X ())]
--commands = defaultCommands

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  h <- spawnPipe "xmobar /home/olivier/.xmonad/xmobar.config"
  xmonad $ defaultConfig {
               terminal           = myTerminal,
               focusFollowsMouse  = myFocusFollowsMouse,
               borderWidth        = myBorderWidth,
               modMask            = myModMask,
               numlockMask        = myNumlockMask,
               workspaces         = myWorkspaces,
               normalBorderColor  = myNormalBorderColor,
               focusedBorderColor = myFocusedBorderColor,

               -- key bindings
               keys               = myKeys,
               mouseBindings      = myMouseBindings,

               -- hooks, layouts
               layoutHook         = myLayout,
               manageHook         = myManageHook,
               logHook            = dynamicLogWithPP xmobarPP {
                                      ppLayout = const "",
                                      ppTitle = xmobarColor "green"  "" . shorten 80,
                                      ppOutput = hPutStrLn h }
             }
