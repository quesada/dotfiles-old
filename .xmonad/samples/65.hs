-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Remi%27s_xmonad.hs
import Data.Ratio
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Arrow hiding ((|||))
import System.Exit
import System.IO
import XMonad
import qualified XMonad.StackSet as W
import Data.IORef

----- xmonad-contrib -----
import qualified XMonad.Actions.FlexibleResize as FlexibleResize
import XMonad.Actions.CycleWS

import XMonad.Layout.DragPane (dragPane, DragType(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.LayoutHints (layoutHints)

-- xprop -f _NET_WM_STRUT 32c -set _NET_WM_STRUT '0, 0, 16, 0'
--import qualified XMonad.Hooks.ManageDocks as ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Hooks.UrgencyHook

import XMonad.Util.Run

main = do
    xmobar <- spawnPipe "xmobar"
    -- the next N new windows will be floated
    floatNextWindows <- newIORef 0
    xmonad $ withUrgencyHook dzenUrgencyHook $ defaultConfig
        { terminal      = "urxvt"
        , layoutHook    = layoutH
        , manageHook    = manageH floatNextWindows
        , workspaces    = ["music", "chat", "web"] ++ map show [4 .. 9 :: Int]
        , defaultGaps   = [(16,0,0,0)]
        , modMask       = mod4Mask
        , keys          = keyB floatNextWindows
        , mouseBindings = mouseB
        , logHook       = dynamicLogWithPP $ defaultPP
                                {ppCurrent  = xmobarColor "yellow" ""
                                ,ppHidden   = const ""
                                ,ppLayout   = \s -> case fromMaybe s $ stripPrefix "Hinted " s of
                                               "Mirror Tall"    -> "Wide"
                                               s                -> s
                                ,ppSep      = " | "
                                ,ppTitle    = xmobarColor "#aaaabb" "" . shorten 40
                                ,ppOutput   = hPutStrLn xmobar
                                ,ppExtras   = [do
                                                    i <- io $ readIORef floatNextWindows
                                                    return $ Just $ if i == 0
                                                                        then "-"
                                                                        else show i
                                              ]
                                }
        }

layoutH = layoutHints . smartBorders
        $ onWorkspace "music"   (tiled
                                ||| Mirror tiled
                                ||| Full
                                )
        $ onWorkspace "chat"    (tiled
                                ||| Mirror tiled
                                ||| Full
                                )
        $ onWorkspace "web"     (Mirror tiled2
                                ||| tiled2
                                ||| Full
                                )
        $ Mirror tiled
            ||| tiled
            ||| Full
            ||| dragPane Horizontal 0.1 0.5
  where
     tiled  = Tall 1 (3 % 100) (3 % 5)
     tiled2 = Tall 1 (3 % 100) (4 % 5)

manageH :: IORef Integer -> ManageHook
manageH floatNextWindows = composeAll $ concat
                [[ className =? name        --> doFloat                 | name <- floats ]
                ,[ className =? name        --> doF (W.shift workspace) | (name, workspace) <- shifts ]
                ,[ resource  =? res         --> doIgnore                | res <- ignores ]
                ,[ (> 0) `liftM` io (readIORef floatNextWindows)
                                            --> do io (modifyIORef floatNextWindows pred) >> doCenterFloat ]
                ]
    where
        floats  = ["MPlayer", "Gimp", "Blender", "Xmessage", "Cinelerra", "foobillard", "xaos", "Ddd", "xine"]
        shifts  = ("Firefox-bin",   "web")
                : ("Pidgin",        "chat")
                : zip ["Qmpdclient", "Gmpc", "Sonata"] (repeat "music")
        ignores = ["desktop_window", "kdesktop"]

keyB :: IORef Integer -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
keyB floatNextWindows conf@(XConfig {modMask = modMask}) = Map.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_z     ), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask .|. shiftMask, xK_z     ), runInTerm "" "zsh -l") -- %! Launch terminal
    , ((modMask,               xK_a     ), spawn "firefox") -- %! Launch a firefox
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec urxvt -e $exe\"") -- %! Launch dmenu
    , ((modMask,               xK_x     ), kill) -- %! Close the focused window
    , ((modMask .|. shiftMask, xK_x     ), withDisplay $ \d ->
                                            withFocused $ \w -> io $ do
                                                killClient d w
                                                return ()
                                            ) -- %! Kill the focused window
    , ((modMask,            xK_KP_Right ), spawn "mpc next")
    , ((modMask,            xK_KP_Left  ), spawn "mpc prev")
    , ((modMask,            xK_KP_Up    ), spawn "mpc toggle")
    , ((modMask,            xK_KP_Down  ), spawn "mpc play")
    , ((modMask,            xK_Pause    ), spawn "xscreensaver-command -lock")
    , ((modMask,            xK_Print    ), spawn "scrot -q10 'shot-%Y%m%d-%H.%M.%S.png'")
    , ((modMask .|. shiftMask, xK_Print ), spawn "sleep 0.2; scrot -q10 -s 'shot-%Y%m%d-%H.%M.%S.png'")
    , ((modMask,            xK_u        ), runInTerm "" "mutt")

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size
    , ((modMask,               xK_f     ), io (modifyIORef floatNextWindows succ) >> logHook conf)
    , ((modMask .|. shiftMask, xK_f     ), io (modifyIORef floatNextWindows (const 0)) >> logHook conf)

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
    , ((modMask              , xK_b     ), modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True) -- %! Restart xmonad

    -- edit xmonad.hs
    , ((modMask              , xK_e     ), spawn "gvim $HOME/.xmonad/xmonad.hs") -- %! edit xmonad.hs

    ]
    -- goto and/or shift to the next/prev workspace
    ++ prevNextWorkspaceBindings modMask xK_Left xK_Right
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    -- mod-shift-control-[1..9] %! Move client and switch to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0)
                    ,(W.shift, shiftMask)
                    ,(\i -> W.greedyView i . W.shift i, shiftMask .|. controlMask)
                    ]]

-- Not needing a mouse doesn't mean not using it
button6     =  6 :: Button
button7     =  7 :: Button
button8     =  8 :: Button
button9     =  9 :: Button
button10    = 10 :: Button
button11    = 11 :: Button
button12    = 12 :: Button
button13    = 13 :: Button
button14    = 14 :: Button
button15    = 15 :: Button

mouseB :: XConfig Layout -> Map (KeyMask, Button) (Window -> X ())
mouseB (XConfig {XMonad.modMask = modMask}) = Map.fromList $
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask                 , button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask                 , button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask                 , button3), \w -> focus w >> FlexibleResize.mouseResizeWindow w)

    -- Mouse scroll wheel to raise/lower windows
    , ((modMask                 , button5), \w -> windows W.swapDown)
    , ((modMask                 , button4), \w -> windows W.swapUp  )

    , ((0                       , button9), \w -> spawn "mpc next")
    , ((0                       , button8), \w -> spawn "mpc prev")
    , ((0                       , button10),\w -> spawn "mpc toggle")
    , ((modMask                 , button10),\w -> spawn "mpc random")
    ]
    -- Horizontal scroll wheel for next/prev workspace
    ++ map (second const) (prevNextWorkspaceBindings modMask button13 button14)

prevNextWorkspaceBindings :: KeyMask -> a -> a -> [((KeyMask, a), X ())]
prevNextWorkspaceBindings modMask prev next =
    [ ((modMask,               next),   nextWS)
    , ((modMask,               prev),   prevWS)
    , ((modMask .|. shiftMask, next),   shiftToNext)
    , ((modMask .|. shiftMask, prev),   shiftToPrev)
    , ((modMask .|. shiftMask .|. controlMask
                             , next),   shiftToNext >> nextWS)
    , ((modMask .|. shiftMask .|. controlMask
                             , prev),   shiftToPrev >> prevWS)
    ]
