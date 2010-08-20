-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/deifl%27s_xmonad.hs
import XMonad
import Graphics.X11.Xlib
import XMonad.Operations
import XMonad.Layouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Bits
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)
import XMonad.Actions.WindowBringer
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import qualified XMonad.StackSet as W

main =  xmonad $ defaultConfig
       { borderWidth        = 1
       , terminal           = "urxvt"
       , normalBorderColor  = "#333333"
       , modMask            = mod4Mask 
       , defaultGaps        = [(15,15,0,0)]
       , focusedBorderColor = "#fedb73" 
       , logHook            = dynamicLogWithPP deiflPP
       , keys = \c -> mykeys c `M.union` keys defaultConfig c
       , layoutHook         = tiled ||| Mirror tiled ||| Full ||| tabbed shrinkText myTabConfig
       } 
    where
      -- default tiling algorithm partitions the screen into two panes
      -- tiled   = Tall nmaster delta ratio
      tiled   = ResizableTall nmaster delta ratio []
          
      -- The default number of windows in the master pane
      nmaster = 2
                                 
      -- Default proportion of screen occupied by master pane
      ratio   = 0.618034
                                           
      -- Percent of screen to increment by when resizing panes
      delta   = 3/100 
                
      -- Additional keybindings
      mykeys c@(XConfig {modMask = modm}) = M.fromList $
                                            [ ((modm,               xK_x     ), sendMessage MirrorShrink)
                                            , ((modm,               xK_s     ), sendMessage MirrorExpand)
                                            , ((modm .|. shiftMask, xK_g     ), gotoMenu)
                                            , ((modm .|. shiftMask, xK_b     ), bringMenu)
                                            , ((modm,               xK_p), spawn "exe=`dmenu_path | dmenu -b -p 'Launch:' -fn '-xos4-terminus-medium-r-normal-*-12-*-*-*-*-*-*-*'  -nb '#333' -nf '#888' -sb black -sf '#a8a8ff'` && eval \"exec $exe\"")
                                            , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)
                                            , ((modm .|. shiftMask, xK_Left),  shiftToPrev >> prevWS)
                                            , ((modm,               xK_Up),    toggleWS)
                                            , ((modm,               xK_Right), prevWS)
                                            , ((modm,               xK_Left),  nextWS)
                                            , ((modm .|. shiftMask, xK_c), kill1)
                                            ]
                                            -- CopyWindow (still don't work
                                            ++
                                            -- mod-[1..9] @@ Switch to workspace N
                                            -- mod-shift-[1..9] @@ Move client to workspace N
                                            -- mod-control-shift-[1..9] @@ Copy client to workspace N
                                            [((m .|. modm, k), windows $ f i)
                                                | (i, k) <- zip (workspaces c) [xK_1 ..]
                                                , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]

myTabConfig :: TConf
myTabConfig = defaultTConf { inactiveBorderColor = "#708090"
                           , activeBorderColor = "#5f9ea0"
                           , activeColor = "#000000"
                           , inactiveColor = "#333333"
                           , inactiveTextColor = "#888888"
                           , activeTextColor = "#87cefa"
                           , fontName = "-xos4-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
                           , tabSize = 15
                           }

deiflPP :: PP
deiflPP = defaultPP { ppCurrent = wrap "^bg(#000)^fg(#a8a8ff) " " ^fg(#fedb73)^bg(#333)" 
                    , ppSep     = ""
                    , ppWsSep = "|"
                    , ppVisible = wrap "^bg(#000)^fg(#888) " " ^bg(#333)"
                    , ppLayout = (\_ -> "")
                    , ppHidden = wrap "^bg(#333)^fg(#888) " " "
                    }</haskell>
0

