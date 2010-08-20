-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Eric_Mertens%27_xmonad.hs
-- xmonad.hs
import XMonad
import XMonad.Actions.RotView    ( rotView )
import XMonad.Hooks.DynamicLog   ( PP(..), dynamicLogWithPP, dzenColor, wrap, defaultPP )
import XMonad.Layouts            ( (|||), Full(..) )
import XMonad.Layout.LayoutHints ( layoutHints )
import XMonad.Layout.TilePrime   ( TilePrime(..) )
import XMonad.Prompt             ( defaultXPConfig, XPConfig(..), XPPosition(..) )
import XMonad.Prompt.Shell       ( shellPrompt )
import XMonad.Util.Run

import qualified Data.Map as Map
import Data.Ratio
import Graphics.X11
import System.IO

statusBarCmd= "dzen2 -e '' -w 1040 -ta l -fg white -bg \"#222222\" -fn \"-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-1\""

main = do din <- spawnPipe statusBarCmd
          xmonad $ defaultConfig
                     { borderWidth        = 2
                     , normalBorderColor  = "#666666"
                     , focusedBorderColor = "#cd8b00"
                     , terminal           = "urxvtc"
                     , workspaces         = ["main","net","www"]
                                            ++ map show [4..9]
                     , defaultGaps        = [(18,0,0,0)]
                     , modMask            = mod4Mask
                     , logHook            = dynamicLogWithPP $ myPP din
                     , mouseBindings      = \c -> myMouse `Map.union`
                                                  mouseBindings defaultConfig c
                     , keys               = \c -> myKeys `Map.union`
                                                  keys defaultConfig c
                     , layoutHook         = Layout
                                               $ TilePrime 1 (3%100) (1%2) False
                                             ||| TilePrime 1 (3%100) (1%2) True
                                             ||| layoutHints Full
                     }

myKeys = Map.fromList $
  [ ((mod4Mask, xK_p), shellPrompt myPromptConfig)
  , ((0, 0x1008ff12), spawn "amixer -q set Front toggle")
  , ((0, 0x1008ff13), spawn "amixer -q set PCM 2dB+")
  , ((0, 0x1008ff11), spawn "amixer -q set PCM 2dB-")
  ]

myPromptConfig = defaultXPConfig
                   { position = Top
                   , promptBorderWidth = 0
                   }

myMouse = Map.fromList $
    [ ((mod4Mask, button4), (\_ -> rotView True))
    , ((mod4Mask, button5), (\_ -> rotView False))
    ]

myPP h = defaultPP
         { ppCurrent  = dzenColor "white" "#cd8b00" . pad
         , ppVisible  = dzenColor "white" "#666666" . pad
         , ppHidden   = dzenColor "black" "#cccccc" . pad
         , ppHiddenNoWindows = dzenColor "#999999" "#cccccc" . pad
         , ppWsSep    = dzenColor "#bbbbbb" "#cccccc" "^r(1x18)"
         , ppSep      = dzenColor "#bbbbbb" "#cccccc" "^r(1x18)"
         , ppLayout   = dzenColor "black" "#cccccc" .
                        (\ x -> case x of
                                  "TilePrime Horizontal" ->
                                    " ^i(/home/emertens/images/tile_horz.xpm) "
                                  "TilePrime Vertical"   ->
                                    " ^i(/home/emertens/images/tile_vert.xpm) "
                                  "Hinted Full"          ->
                                    " ^i(/home/emertens/images/fullscreen.xpm) "
                                  _                      -> pad x
                        )
         , ppTitle    = (' ':) . escape
         , ppOutput   = hPutStrLn h
         }
  where
  escape = concatMap (\x -> if x == '^' then "^^" else [x])
  pad = wrap " " " "
