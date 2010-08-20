-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Lee_Aylward%27s_xmonad.hs
import XMonad
import XMonad.Operations
import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Actions.NoBorders
import XMonad.Config
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run

import System.IO
import qualified Data.Map as M
import Data.Ratio ((%))

statusBarCmd= "dzen2 -bg '#324c80' -fg '#adbadd' -e '' -ta l -w 400 -fn '-misc-fixed-medium-r-normal-*-14-*-*-*-*-*-*-*'"
rb_pause = "rhythmbox-client --no-start --play-pause"
rb_volup = "rhythmbox-client --no-start --volume-up"
rb_voldown = "rhythmbox-client --no-start --volume-down"

main = do din <- spawnPipe statusBarCmd
          xmonad $ defaultConfig
                     { borderWidth        = 2
                     , defaultGaps        = [(16,0,0,0)]
                     , modMask            = mod4Mask
                     , normalBorderColor  = "#cccccc"
                     , layoutHook         = resizetall ||| Mirror resizetall ||| noBorders Full
                     , focusedBorderColor = "#cd8b00"
                     , terminal           = "urxvt"
                     , logHook            = dynamicLogWithPP $ dzenPP
                                          { ppOutput = hPutStrLn din }
                     , keys = \c -> mykeys c `M.union` keys defaultConfig c
                     }
 where
     resizetall = ResizableTall 1 (3/100) (1/2) []
     mykeys (XConfig {modMask = modm}) = M.fromList $
             [ ((modm .|. shiftMask, xK_p), spawn rb_pause)
             , ((modm .|. shiftMask, xK_equal), spawn rb_volup)
             , ((modm .|. shiftMask, xK_minus), spawn rb_voldown)
             , ((modm, xK_z), sendMessage MirrorShrink)
             , ((modm, xK_a), sendMessage MirrorExpand)
             , ((modm, xK_g), withFocused toggleBorder) ]
