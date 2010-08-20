-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/entropies_xmonad.hs

{-# OPTIONS_GHC -fglasgow-exts #-}
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
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import XMonad.Util.XSelection
import XMonad.Actions.DwmPromote
import XMonad.Actions.Submap
import XMonad.Actions.RotView
import XMonad.Layout.NoBorders
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation
import XMonad.Actions.CycleWS
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.HintedTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.Maximize
import XMonad.Layout.Tabbed

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask
 
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
myWorkspaces    = ["code","com","web","4","5","6","7","8","music"]
 
-- Border colors for unfocused and focused windows, respectively.
--
--myNormalBorderColor  = "#009A39"
--myNormalBorderColor  = "#009A39"
--myFocusedBorderColor = "#FF7D42"
myNormalBorderColor  = "#005E20"
myFocusedBorderColor = "#FF6200"
-- Default offset of drawable screen boundaries from each physical
-- screen. Anything non-zero here will leave a gap of that many pixels
-- on the given edge, on the that screen. A useful gap at top of screen
-- for a menu bar (e.g. 15)
--
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
myDefaultGaps   = [(16,0,0,0), (25,0,0,0)]
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask,    xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,                  xK_Return), dwmpromote)
    , ((modMask,                  xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask .|. shiftMask,    xK_p     ), spawn "gmrun")
    , ((modMask .|. shiftMask,    xK_c     ), kill)
    , ((modMask,                  xK_r     ), sendMessage $ Toggle MIRROR)
    , ((modMask,                  xK_f     ), sendMessage $ Toggle FULL)
    , ((modMask .|. controlMask,  xK_f     ), withFocused (sendMessage . maximizeRestore))
    , ((modMask .|. shiftMask,    xK_f     ), sendMessage $ Toggle NOBORDERS)
    , ((modMask,                  xK_o     ), sendMessage $ Toggle TABBED)
    , ((modMask,                  xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,    xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,                  xK_n     ), refresh)
    , ((modMask,                  xK_k     ), windows W.focusUp)
    , ((modMask,                  xK_j     ), windows W.focusDown)
    , ((modMask,                  xK_h     ), sendMessage Shrink)
    , ((modMask,                  xK_l     ), sendMessage Expand)
    , ((modMask,                  xK_t     ), withFocused $ windows . W.sink)
    , ((modMask,                  xK_m     ), rotView True)
    , ((modMask,                  xK_n     ), rotView False)
    , ((modMask,                  xK_s     ), toggleWS)
    , ((modMask .|. shiftMask,    xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask .|. shiftMask,    xK_period), sendMessage (IncMasterN (-1)))
    , ((modMask .|. controlMask,  xK_plus  ), sendMessage MagnifyMore)
    , ((modMask .|. controlMask,  xK_minus ), sendMessage MagnifyLess)
    , ((modMask .|. controlMask,  xK_o     ), sendMessage ToggleOff)
    , ((modMask .|. controlMask .|. shiftMask,  xK_o), sendMessage ToggleOn)
    -- , ((modMask .|. controlMask,  xK_l      ), withFocused (sendMessage . expandWindowAlt))
    -- , ((modMask .|. controlMask,  xK_h      ), withFocused (sendMessage . shrinkWindowAlt))
    -- , ((modMask .|. controlMask,  xK_j      ), withFocused (sendMessage . tallWindowAlt))
    -- , ((modMask .|. controlMask,  xK_k      ), withFocused (sendMessage . wideWindowAlt))
    -- , ((modMask .|. controlMask,  xK_space  ), sendMessage resetAlt)
    -- toggle the status bar gap
    , ((modMask              , xK_b         ),
        modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
                           in if n == x then (0,0,0,0) else x))
 
    , ((modMask, xK_a), submap . M.fromList $
        [   ((0, xK_n),     spawn "mpc next")
          , ((0, xK_p),     spawn "mpc prev")
          , ((0, xK_z),     spawn "mpc random")
          , ((0, xK_s),     spawn "mpc stop")
          , ((0, xK_0),     spawn "rmixer both +7")
          , ((0, xK_9),     spawn "rmixer both -7")
          , ((0, xK_space), spawn "mpc toggle")
        ])
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    -- , ((modMask , xK_q     ), broadcastMessage ReleaseResources >> restart (Just "xmonad") True)
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_period, xK_comma] [0..]
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

data MIRROR = MIRROR deriving (Read, Show, Eq, Typeable)
instance Transformer MIRROR Window where
     transform _ x k = k (Mirror x)

data NOBORDERS = NOBORDERS deriving (Read, Show, Eq, Typeable)
instance Transformer NOBORDERS Window where
     transform _ x k = k (noBorders x)
 
data FULL = FULL deriving (Read, Show, Eq, Typeable)
instance Transformer FULL Window where
     transform _ x k = k (noBorders Full)

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
     transform _ x k = k (tabbed shrinkText defaultTConf)

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
-- myLayout = maximize ( toggleLayouts (noBorders Full) ( magnifier ( Mirror tiled ) ||| MagicFocus( Dishes 1 (1/6) ) ||| magnifier( Grid ) ||| noBorders Full))
myLayout = id
         . smartBorders
         . mkToggle (NOBORDERS ?? FULL ?? EOT)
         . mkToggle (TABBED ?? EOT)
         . mkToggle (MIRROR ?? EOT)
         $ ( layoutHints(HintedTile 1 0.1 0.6 XMonad.Layout.HintedTile.Wide)
           ||| magnifier(HintedTile 1 0.1 0.6 XMonad.Layout.HintedTile.Wide)
           ||| layoutHints(XMonad.Tall 1 (3/100) (3/5))
           -- ||| windowNavigation (combineTwo (dragPane Horizontal 0.1 0.314) (HintedTile 1 0.1 0.5 XMonad.Layout.HintedTile.Tall) (HintedTile 1 0.1 0.5 XMonad.Layout.HintedTile.Tall) )
           )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = XMonad.Tall nmaster delta ratio
     nmaster = 1
      -- Default proportion of screen occupied by master pane
--     ratio   = 3/5
     ratio   = toRational (2/(1+sqrt(5)::Double))
     -- Percent of screen to increment by when resizing panes
     delta   = 4/100

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
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kicker"         --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myLogHook = dynamicLogWithPP myPP
  where
  myPP = defaultPP { ppCurrent  = dzenColor "#102A39" "#CC7605" . pad
                   , ppVisible  = dzenColor "#CC7605" "#102A39" . pad
                   , ppHidden   = dzenColor "#989898" "#102A39" . pad
                   , ppHiddenNoWindows = const ""
                   , ppWsSep    = ""
                   , ppSep      = " * "
                   , ppTitle    = dzenColor "#29B74B" "#102A39" . (' ':) . escape
                   , ppLayout   = const "" --dzenColor "#124A73" "#102A39" . pad
                   }
  escape = concatMap (\x -> if x == '^' then "^^" else [x])
  pad = wrap " " " "

------------------------------------------------------------------------
- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad defaults
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        defaultGaps        = myDefaultGaps,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook
    }


