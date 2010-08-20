-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/twifkak%27s_xmonad.hs
{-# LANGUAGE PatternGuards #-}

-- XMonad Core
import XMonad
import qualified XMonad.StackSet as W

-- GHC hierarchical libraries
import qualified Data.Map as M
import Data.IORef

-- Contribs
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Submap
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Util.EZConfig

main = (xmonad =<<)
       $ withWindowNavigation (xK_w, xK_a, xK_s, xK_d)
       $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"],
                                            duration = seconds 1 }
                          urgencyConfig { remindWhen = Every (1 `minutes`) } -- only in post-0.8 darcs
       $ defaultConfig
         { normalBorderColor  = "#222222"
         , focusedBorderColor = "#AA0000"
         , workspaces         = workspaces'
         , modMask            = modMask'
         , numlockMask        = 0
         , layoutHook         = layoutHook'
         , focusFollowsMouse  = False
         , terminal           = "urxvtc || urxvt"
         , mouseBindings      = mouseBindings'
         , startupHook        = setWMName "LG3D"
         }
         `additionalKeys` keys'

modMask'    = mod4Mask

workspaces' = map show $ [1 .. 9 :: Int] ++ [0]

layoutHook' =
    configurableNavigation noNavigateBorders $
    smartBorders $
    layouts

layouts =
        tiled
    ||| Mirror tiled
    ||| noBorders (tabbedAlways shrinkText
                          defaultTheme { fontName = "-*-terminus-medium-r-normal--12-*-iso8859-1" })
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 2     -- The default number of windows in the master pane
     ratio   = 1/2   -- Default proportion of screen occupied by master pane
     delta   = 3/100 -- Percent of screen to increment by when resizing panes

xpConfig = defaultXPConfig { position = Top }
winXpConfig = xpConfig { autoComplete = Just (seconds 0.5) }

keys' =
    [ ((modMask'              , xK_p        ), shellPrompt xpConfig )
    , ((modMask'              , xK_F1       ), spawn "date | dzen2 -p 2 -xs 1") -- %! Print current date
    , ((modMask' .|. mod1Mask , xK_m        ), submap $ M.fromList -- %! MPD prefix key
        [ ((modMask' .|. mod1Mask,  xK_p        ), spawn "mpc toggle") -- %! MPD: Toggle pause/play
        , ((modMask' .|. mod1Mask,  xK_m        ), spawn "mpc | head -1 | dzen2 -p 2 -xs 1") -- %! MPD: Print the currently playing song
        , ((modMask' .|. mod1Mask,  xK_comma    ), spawn "mpc prev") -- %! MPD: Go to previous song
        , ((modMask' .|. mod1Mask,  xK_period   ), spawn "mpc next") -- %! MPD: Go to next song
        ] )
    , ((modMask' .|. mod1Mask,  xK_space    ), withFocused $ \w -> hide w >> reveal w >> setFocusX w) -- %! force the window to redraw itself
    , ((modMask'              , xK_Return   ), promote)
    , ((modMask'              , xK_i        ), prevWS)
    , ((modMask'              , xK_o        ), nextWS)
    , ((modMask' .|. shiftMask, xK_i        ), shiftToPrev)
    , ((modMask' .|. shiftMask, xK_o        ), shiftToNext)
    , ((modMask' .|. mod1Mask , xK_i        ), swapTo Prev)
    , ((modMask' .|. mod1Mask , xK_o        ), swapTo Next)
    , ((modMask'              , xK_c        ), windowPromptGoto  winXpConfig)
    , ((modMask'              , xK_b        ), windowPromptBring winXpConfig)
    , ((modMask'              , xK_BackSpace), focusUrgent)
    , ((modMask' .|. shiftMask, xK_BackSpace), clearUrgents) -- only in post-0.8 darcs

    -- Fake multimonitor
    , ((modMask' .|. mod1Mask , xK_n        ), layoutScreens 2 (TwoPane 0.5 0.5))
    , ((modMask' .|. mod1Mask , xK_b        ), rescreen)
    ]
    ++
    -- modMask'-[1..0] %! Switch to workspace N
    -- modMask'-shift-[1..0] %! Move client to workspace N
    [((m .|. modMask', k), windows $ f i)
        | (i, k) <- zip workspaces' $ [xK_1 .. xK_9] ++ [xK_0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- modMask'-{e,r} %! Switch to physical/Xinerama screens 1 or 2
    -- modMask'-shift-{e,r} %! Move client to screen 1 or 2
    [((m .|. modMask', key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [((modMask' .|. mod1Mask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip workspaces' $ [xK_1 .. xK_9] ++ [xK_0]]

-- I have "Emulate three button mouse" turned on in Darwin X11, so mod4 (Apple) right-clicks.
-- Use ctrl-shift, instead.
mouseBindings' (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- ctrl-shift-button1 %! Set the window to floating mode and move by dragging
    [ ((controlMask .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >>
                                                               windows W.shiftMaster))

    -- ctrl-shift-button2 %! Raise the window to the top of the stack
    , ((controlMask .|. shiftMask, button2), (\w -> focus w >> windows W.shiftMaster))

    -- ctrl-shift-button3 %! Set the window to floating mode and resize by dragging
    , ((controlMask .|. shiftMask, button3), (\w -> focus w >> mouseResizeWindow w >>
                                                               windows W.shiftMaster))
    ]
