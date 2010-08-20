-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Ray%27s_xmonad.hs
import XMonad
import XMonad.Layout
import XMonad.Operations
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import XMonad.Util.Run
import XMonad.Actions.NoBorders
import XMonad.Actions.Warp
import qualified XMonad.StackSet as W
import Graphics.X11
import qualified Data.Map as M
import System.IO.UTF8
import Data.Ratio
import Data.Bits

main = do dzenh <- spawnPipe "dzen2 -bg '#333333'"
          xmonad $ rayConfig dzenh

rayConfig h = defaultConfig
              { normalBorderColor        = "#333333"
              , focusedBorderColor       = "#0000ff"
              , borderWidth              = 1
              , workspaces               = ["α","β","γ","δ","ε","ζ"]
              , terminal                 = "rayterm"
              , defaultGaps              = [(18,0,0,0)]
              , modMask                  = mod4Mask
              , manageHook               = rayManageHook
              , logHook                  = dynamicLogWithPP $ rayPP h
              , keys                     = \c -> rayKeys c `M.union` keys defaultConfig c
              , focusFollowsMouse        = False
              , layoutHook               = Mirror (Tall 1 0 (1%2))
                                           ||| noBorders Full
              }

rayManageHook = composeAll . concat $
                [ [ className =? c --> doFloat | c <- floats]
                , [ className =? "Opera" --> doF (W.shift "β") ]]
    where floats = ["MPlayer", "Gimp","xli","Xmessage"]

rayKeys (XConfig {modMask = modm}) = M.fromList $
                                     [ ((modm                , xK_s), sshPrompt defaultXPConfig)
                                     , ((modm .|. controlMask, xK_x), xmonadPrompt defaultXPConfig)
                                     , ((modm                , xK_x), spawn "xscreensaver-command -lock")
                                     , ((modm .|. shiftMask  , xK_b), withFocused toggleBorder)
                                     , ((modm .|. controlMask, xK_b), warpToScreen 0 1 1)
                                     ]

rayPP h = PP { ppCurrent         = wrap "^fg(#ff0000)\"" "\"^fg()"
             , ppVisible         = wrap "^fg(#a00000)\"" "\"^fg()"
             , ppHidden          = wrap "^fg(#ffffff)\"" "\"^fg()"
             , ppHiddenNoWindows = wrap "^fg(#555555)\"" "\"^fg()"
             , ppSep             = "   "
             , ppWsSep           = ","
             , ppTitle           = shorten 80
             , ppLayout          = (++) "layout = "
             , ppOrder           = (take 2) . workspaceTag
             , ppOutput          = System.IO.UTF8.hPutStrLn h
             }
    where workspaceTag (x:xs) = ("workspaces = [" ++ x ++ "]") : xs</haskell>
0

