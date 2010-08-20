-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/andrewsw%27s_xmonad.hs
import XMonad
import XMonad.Layout
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)
import XMonad.Prompt
import XMonad.Prompt.Ssh
import qualified Data.Map as M
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import Data.Ratio
import XMonad.Layout.LayoutHints


myDefaultGaps = [(15,0,0,0)]

myNormalBorderColor = "#000000"
myFocusedBorderColor = "#eef204"

myManageHook = composeAll [ className =? "XCalc" --> doFloat
                          , className =? "display" --> doFloat ]

newManageHook = myManageHook <+> manageHook defaultConfig

myKeys x =
    [ ((modMask x .|. controlMask, xK_s), sshPrompt defaultXPConfig)
    , ((modMask x .|. shiftMask, xK_f), spawn "mpc toggle") -- toggle mpc playing or not                                      
    , ((modMask x              , xK_v), spawn "mpc volume +10") -- raise volume                                               
    , ((modMask x .|. shiftMask, xK_v), spawn "mpc volume -10") -- lower volume                                               
    , ((modMask x .|. controlMask .|. shiftMask, xK_u), spawn "sudo s2disk") -- suspend to disk                               
    ]

newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))


myLayoutHook = layoutHints (tiled ||| Mirror tiled ||| Circle ||| magnify Grid ||| Full)

    where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      delta = 3/100
      ratio = 1/2
      magnify = magnifiercz (12%10)





main = do

  xmobar <- spawnPipe "xmobar"
  xmonad $ defaultConfig
             { borderWidth = 2
             , modMask = mod3Mask
             , defaultGaps = myDefaultGaps
             , manageHook = newManageHook
             , logHook = dynamicLogWithPP defaultPP { ppOutput = hPutStrLn xmobar}
             , keys = newKeys
             , layoutHook = myLayoutHook
             , normalBorderColor = myNormalBorderColor
             , focusedBorderColor = myFocusedBorderColor
             }
