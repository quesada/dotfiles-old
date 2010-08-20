-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/sykopomp%27s_xmonad.hs
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
 
import qualified XMonad.StackSet as W 
import Data.Bits ((.|.))
import System.Exit
import System.IO
import qualified Data.Map as M
 
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Circle
-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Submap
import XMonad.Actions.DwmPromote
--Utils
import XMonad.Util.EZConfig
 
 
main = xmonad $ defaultConfig
       { borderWidth		= 2
       , focusedBorderColor 	= "#C11B17"
       , normalBorderColor 	= "#2e3436"
       , manageHook    	= myManageHook <+> manageDocks
       , workspaces    	= map show [1 .. 9 :: Int]
       , terminal	= "urxvtc"
       , modMask       	= mod4Mask
       , logHook       	= myLogHook
       , layoutHook    	= windowNavigation $ (avoidStruts (myTab ||| tall ||| Mirror tall ||| Circle))
       , keys 		= myKeys

       }

    where 
      
      tall 	= ResizableTall 1 (3/100) (1/2) []


myTab = tabbed shrinkText myTabConfig
 
-- The tab layout config {{{

myTabConfig = defaultTheme
    { activeColor         = "#C11B17"
    , inactiveColor       = "#7E2217"
    , urgentColor	  = "#C500C5"
    , activeBorderColor   = "white"
    , inactiveBorderColor = "grey"
    , activeTextColor     = "white"
    , inactiveTextColor   = "grey"
    , decoHeight          = 12
    , fontName            = "-*-terminus-*-*-*-*-12-*-*-*-*-*-iso10646-1"
    }

myLogHook :: X ()
myLogHook = do ewmhDesktopsLogHook
               return ()
                      
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
myManageHook = composeAll
               [ className =? "MPlayer"        --> doFloat
	       , className =?  "Gimp"           --> doFloat
	       , className =? "Thunar"	    --> doFloat
	       , className =? "VLC media player"	    --> doFloat
               , className =? "Thunderbird-bin" --> doF(W.shift "3")
               , className =? "Pidgin"	    --> doF(W.shift "1")
               , className =? "Minefield"    --> doF(W.shift "2")
               , resource  =? "amarokapp"	    --> doF(W.shift "5")
               , className =? "Gimmix"	    --> doF(W.shift "5")
               , resource  =? "desktop_window" --> doIgnore
               , className =? "Xfce4-panel"    --> doFloat
               , className =? "Xfce-mcs-manager" --> doFloat 
               , className =? "Xfce-mixer"	      --> doFloat
               , className =? "Gui.py"	    --> doFloat
               , manageDocks]

--------------------------
-- emacs-oriented remap --
--------------------------
myKeys = \conf -> mkKeymap conf $
                  --Window Navigation
                  --change focus with arrows                  
                [ ("M-<R>", sendMessage $ Go R)
                , ("M-<L>", sendMessage $ Go L)
                , ("M-<U>", sendMessage $ Go U)
                , ("M-<D>", sendMessage $ Go D)
                
                 --next/previous emacs-like
                , ("M-n", windows W.focusDown)
                , ("M-p", windows W.focusUp)
                , ("M-m", windows W.focusMaster)

                --Window Movement
                  -- swap
                , ("M-S-<R>", sendMessage $ Swap R)
                , ("M-S-<L>", sendMessage $ Swap L)
                , ("M-S-<U>", sendMessage $ Swap U)
                , ("M-S-<D>", sendMessage $ Swap D)
                  
                  --emacs-like
                , ("M-S-n", windows W.swapDown)
                , ("M-S-p", windows W.swapUp)
        
                  --Swap with master/DwmPromote
                , ("M-S-m", windows W.swapMaster)
                , ("M-x p", dwmpromote) --might change this to M-<Return>

                --Shrink/expand
                  --master area
                , ("M-s", sendMessage Shrink)
                , ("M-e", sendMessage Expand)
                  --subareas
                , ("M-S-s", sendMessage MirrorShrink)
                , ("M-S-e", sendMessage MirrorExpand)
                
                --DROP window back into tiling.
                , ("M-d", withFocused $ windows . W.sink)
                  
                -- increase/decrease transparency
                , ("M-t", spawn "transset-df -a --dec .1")
                , ("M-S-t", spawn "transset-df -a --inc .1")

                --kill window
                , ("M-x k", kill)
                , ("M-x M-c", kill)
                , ("M-S-c", kill)
                ]
                --Shift workspaces
                ++ 
                [ (m ++ i, windows $ f j)
                    | (i, j) <- zip (map show [1..9]) (XMonad.workspaces conf)
                    , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)] --Shift wndw to ws
                ]
                ++
                --Xinerama stuff
                [ (m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
                  | (key, sc) <- zip [",", "."] [0..]
                , (m, f) <- [("M-", W.view), ("M-S-", W.shift)]
                ]                
                ++
                --Layout Management
                  --layout toggling
                [ ("M-<Space>", sendMessage NextLayout)
                , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)

                  --change # of windows in master pane
                , ("M-+", sendMessage (IncMasterN 1))
                , ("M--", sendMessage (IncMasterN (-1)))

                --toggle tool gap
                , ("M-x t", sendMessage ToggleStruts)
                ]
                ++
                --XMonad system
                [ ("M-C-<Esc>",    spawn $ "xkill") 
                , ("M-x r", refresh)
                , ("M-S-q",        io (exitWith ExitSuccess))
                , ("M-q",          restart "xmonad" True)
                ]
                ++
                --Applications
                [ ("M-l", spawn  "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
                , ("M-S-<Return>", spawn $ XMonad.terminal conf)
                
                  --Multimedia keys
                , ("M-v", spawn "aumix -v -3")
                , ("M-S-v", spawn "aumix -v +5")
                , ("M-a p", spawn "mpc toggle") --meta-Audio <previous>
                , ("M-a ,", spawn "mpc prev")
                , ("M-a .", spawn "mpc next")

                ]
