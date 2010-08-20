-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/thoughtpolice%27s_xmonad.hs
-- The horrible thing about the Two Minutes Hate was not that 
-- one was obliged to act a part, but that it was impossible 
-- to avoid joining in.
--
import XMonad
import System.Exit
import Control.Monad.State
import Control.Concurrent (forkIO)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified System.IO.UTF8 as UTF8

statusbar = "dzen2 -ta l"
 
main :: IO ()
main = do pipe <- spawnPipe statusbar
          xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
              terminal   = "urxvt +sb -fg white -bg black"
            , workspaces = ["web/irc","code", "reading"] ++ map show [4..9]
            , keys       = myKeys
            , layoutHook = avoidStruts layouts
            , manageHook = composeAll [ manageDocks  -- manage xmobar/dzen2, uses ManageDocks
                                      , className =? "Firefox" --> doF (W.shift "web/irc")
                                      , className =? "Xpdf"    --> doF (W.shift "reading")
                                      , className =? "Xchm"    --> doF (W.shift "reading")]
            , logHook    = dynamicLogWithPP defaultPP { ppCurrent = dzenColor "black" "grey"
                                                      , ppHidden  = dzenColor "grey"  "black" 
                                                      , ppUrgent  = dzenColor "red"   "yellow"
                                                      , ppSep     = "   |||   "
                                                      , ppWsSep   = " * "
                                                      , ppTitle   = shorten 90
                                                      , ppOrder   = reverse 
                                                      , ppOutput  = UTF8.hPutStrLn pipe } }
    where layouts  = smartBorders $ (Mirror $ Tall 1 (3/100) (1/2))
                 ||| simpleTabbed          
                 ||| Full
                        
 
xpc :: XPConfig
xpc = defaultXPConfig { bgColor  = "black"
                      , fgColor  = "grey"
                      , promptBorderWidth = 0
                      , position = Bottom
                      , height   = 15
                      , historySize = 256 }
 
myKeys c = mkKeymap c $                                 -- keys; uses EZConfig
    [ ("M-S-<Return>",  spawn $ XMonad.terminal c)       -- spawn terminal
    , ("M-p"         ,  shellPrompt xpc)                 -- spawn menu program, uses Shell
    , ("M-s"         ,  search)                          -- search websites, uses Search & Submap
    , ("M-S-s"       ,  sshPrompt xpc)                   -- spawn ssh, uses Ssh
    , ("M-S-c"       ,  kill)                            -- kill window
    , ("M-<Space>"   ,  sendMessage NextLayout)          -- next layout
    , ("M-S-<Space>" ,  setLayout $ XMonad.layoutHook c) -- default layout
    , ("M-n"         ,  refresh)                         -- resize to correct size
    , ("M-j"         ,  windows W.focusDown)             -- move focus; next window
    , ("M-k"         ,  windows W.focusUp)               -- move focus; prev. window
    , ("M-m"         ,  windows W.focusMaster)           -- focus on master
    , ("M-<Return>"  ,  windows W.swapMaster)            -- swap current with master
    , ("M-S-j"       ,  windows W.swapDown)              -- swap focused with next window
    , ("M-S-k"       ,  windows W.swapUp)                -- swap focused with prev. window
    , ("M-h"         ,  sendMessage Shrink)              -- shrink master area
    , ("M-l"         ,  sendMessage Expand)              -- expand master area
    , ("M-t"         ,  withFocused $ windows . W.sink)  -- put window back on tiling layer
    , ("M-,"         ,  sendMessage (IncMasterN 1))      -- increase number of windows in master pane
    , ("M-."         ,  sendMessage (IncMasterN (-1)))   -- decrease number of windows in master pane
    , ("M-b"         ,  sendMessage ToggleStruts)        -- toggle status bar gap, uses ManageDocks
    , ("M-C-q"       ,  broadcastMessage ReleaseResources
                        >> restart "xmonad" True)        -- restart xmonad
    , ("C-S-q"       ,  io (exitWith ExitSuccess))       -- exit xmonad
    ] ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [(m ++ k, windows $ f w)
        | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
        , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]
 
 where searchSite = S.promptSearchBrowser xpc "ff3"
       search     = SM.submap . mkKeymap c $
                     [("g", searchSite S.google)
                     ,("h", searchSite S.hoogle)
                     ,("a", searchSite S.amazon)
                     ,("i", searchSite S.imdb)
                     ,("y", searchSite S.youtube)
                     ,("w", searchSite S.wikipedia)]

