-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/enko%27s_xmonad.hs
import XMonad
import XMonad.Layout
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.ThreeColumns
import XMonad.Operations
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import XMonad.Actions.Submap
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Layout.Spiral
import Data.Ratio
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)
import Control.Monad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog hiding (shorten)
import XMonad.Actions.UpdatePointer
import Monad (liftM)
import XMonad
import XMonad.Core
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.TabBarDecoration
import XMonad.Hooks.DynamicLog hiding (shorten)
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.DwmPromote
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)
import System.IO 
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
import XMonad.Actions.Warp
import Data.Ratio
import XMonad.Layout.Circle


shorten :: Int -> String -> String
shorten n xs | length xs < n = xs
             | otherwise     = (take (n - length end) xs) ++ end
 where
    end = "..."



myLogHook h = do
  dynamicLogWithPP $ oxyPP h 
  
oxyPP :: Handle -> PP
oxyPP h = defaultPP  { ppCurrent = wrap "<fc=black,aquamarine3> " " </fc>" 
                     , ppSep     = ""
                     , ppWsSep = ""
                     , ppVisible = wrap "<fc=black,DarkSlateGray4> " " </fc>" 
                     , ppLayout = \x -> "<fc=aquamarine2,black>:: "
                                  ++ case x of
                                       "Mirror ResizableTall"   -> "MTiled"
                                       "ResizableTall"          -> "Tiled"
                                       "Tabbed Bottom Simplest" -> "Tabbed"
                                       "Tabbed Simplest"        -> "Tabbed"
                                       _                        -> x
                                  ++ "</fc> "
                     , ppTitle = \x -> if null x
                                         then ""
                                         else "<fc=DarkSlateGray3,black>[" ++ shorten 83 x ++ "]</fc>"
                     , ppHidden = wrap "<fc=#aaa,black> " " </fc>"
                     , ppOutput = hPutStrLn h
                     }



mykeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
mykeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ 
      ((mod4Mask .|. shiftMask, xK_Return ),  withWindowSet $ \ws->when ((W.tag . W.workspace . W.current)  ws=="term") (spawn "exec urxvt"))
    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((mod4Mask,              xK_l     ), spawn "/home/tim/.local/bin/getseltrans.sh")
    , ((mod4Mask,              xK_c     ), spawn "xbacklight -time 100 + 5")
    , ((mod4Mask,              xK_e     ), spawn "xbacklight -time 100 - 5")
  
    , ((mod4Mask, xK_i), submap . M.fromList $
        [ ((mod4Mask              , xK_n), nextWS)
        , ((mod4Mask              , xK_s), prevWS)
        , ((mod4Mask              , xK_f), viewEmptyWorkspace)
        , ((mod4Mask              , xK_a), addWorkspace "new ws")
        , ((mod4Mask              , xK_d), removeWorkspace)
        , ((mod4Mask              , xK_m), renameWorkspace defaultXPConfig)
        , ((mod4Mask              , xK_x), shellPrompt  defaultXPConfig)
        , ((mod4Mask .|. shiftMask, xK_x), xmonadPrompt  defaultXPConfig)
        , ((mod4Mask .|. shiftMask, xK_v), windowPromptBring defaultXPConfig)
        , ((mod4Mask              , xK_v), windowPromptGoto defaultXPConfig)
        , ((mod4Mask              , xK_u), workspacePrompt defaultXPConfig (windows . W.greedyView))
        , ((mod4Mask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
        , ((mod4Mask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
        , ((mod4Mask              , xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling
        , ((mod4Mask              , xK_h     ), sendMessage Shrink) -- %! Shrink the master area
        , ((mod4Mask              , xK_l     ), sendMessage Expand) -- %! Expand the master area
        , ((mod4Mask              , xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
        , ((mod4Mask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
        , ((mod4Mask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window
        -- , ((mod4Mask              , xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window
        , ((mod4Mask              , xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
        , ((mod4Mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default
        , ((mod4Mask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window
        , ((mod4Mask              , xK_q     ), broadcastMessage ReleaseResources >> restart "xmonad" True) -- %! Restart xmonad

        ])
    , ((mod4Mask, xK_m), submap . M.fromList $
        [ ((0, xK_n),     spawn "xmms2 next")
        , ((0, xK_p),     spawn "xmms2 previous")
        , ((0, xK_space), spawn "xmms2 toggleplay")
        ])

    -- quit, or restart
    
    ]

myManageHook = composeAll
               [ className =? "Amarokapp" --> doF (W.shift "music")
               , className =? "Emacs" --> doF (W.shift "emacs")
               , className =? "Opera" --> doF (W.shift "web")
               , className =? "Kopete" --> doF (W.shift "kopete")
               , className =? "Pidgin" --> doF (W.shift "comm")
               , className =? "Gajim.py" --> doF (W.shift "comm")
               , className =? "MPlayer"        --> doFloat
               ] <+> manageDocks



mylayout = avoidStruts (tiled ||| Mirror tiled ||| spiral (1 % 1) ||| Circle ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 2/3

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

main = do
         xmobar <- spawnPipe "/home/enko/.local/bin/xmobar /home/enko/.xmobar/xmobar.config"
         xmonad $ withUrgencyHook NoUrgencyHook
                $ defaultConfig
                     { borderWidth        = 0
                     , terminal           = "urxvt"
                     , keys = mykeys
                     , workspaces = ["web", "emacs", "comm", "term", "music", "irc", "kx" ]
                     , logHook = myLogHook xmobar
                     , layoutHook = mylayout
                     , manageHook = myManageHook
                  }
