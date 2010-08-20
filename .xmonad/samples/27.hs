-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/lithis%27s_xmonad.hs
import XMonad hiding (Tall)

import XMonad.Actions.Promote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutHints
import XMonad.Layout.HintedTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Run (spawnPipe)

import qualified Data.Map as M
import Data.Ratio
import System.IO (hPutStrLn)
import GHC.IOBase (Handle)

main :: IO ()
main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { normalBorderColor  = backgroundColor
        , focusedBorderColor = focusColor
        , terminal = "$XTERMCMD"
        , layoutHook = myLayout
        , manageHook = manageDocks
        , modMask = mod4Mask
        , borderWidth = 2
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , logHook = dynamicLogWithPP (myPP xmobar)
                 >> updatePointer (Relative 1 1)
        }
        where
            myLayout = layoutHints $ avoidStruts $ smartBorders $ hintedTile Tall
                                 ||| hintedTile Wide
                                 ||| Full
                                 ||| tabbed shrinkText myTheme
                                 ||| spiral (1 % 1)
            hintedTile = HintedTile nmaster delta ratio TopLeft
            nmaster = 1
            ratio   = 1/2
            delta   = 3/100

            myPP :: Handle -> PP
            myPP din = defaultPP
                { ppCurrent = xmobarColor focusColor ""
                , ppVisible = xmobarColor lightTextColor ""
                , ppHiddenNoWindows = xmobarColor lightBackgroundColor ""
                , ppUrgent = xmobarColor urgentColor ""
                , ppSep = " · "
                , ppWsSep = ""
                , ppTitle = xmobarColor lightTextColor ""
                , ppOutput = hPutStrLn din
                }

            myTheme :: Theme
            myTheme = defaultTheme
                { activeColor = lightBackgroundColor
                , inactiveColor = backgroundColor
                , urgentColor = backgroundColor
                , activeBorderColor = textColor
                , inactiveTextColor = textColor
                , urgentTextColor = textColor
                , inactiveBorderColor = lightBackgroundColor
                , urgentBorderColor = urgentColor
                , activeTextColor = lightTextColor
                , fontName = myFont
                }

            myXPConfig :: XPConfig
            myXPConfig = defaultXPConfig
                { font        = myFont
                , bgColor     = backgroundColor
                , fgColor     = textColor
                , fgHLight    = lightTextColor
                , bgHLight    = lightBackgroundColor
                , borderColor = lightBackgroundColor
                }

            myFont = "xft:DejaVu Sans:size=10"
            focusColor = "#60ff45"
            textColor = "#c0c0a0"
            lightTextColor = "#fffff0"
            backgroundColor = "#304520"
            lightBackgroundColor = "#456030"
            urgentColor = "#ffc000"

            myKeys conf@(XConfig {XMonad.modMask = modMask, workspaces = ws}) = M.fromList $
                [ ((modMask,                 xK_Return), promote)
                , ((modMask,                 xK_b), sendMessage ToggleStruts)
                , ((modMask .|. controlMask, xK_x), shellPrompt myXPConfig)
                , ((modMask .|. controlMask, xK_s), sshPrompt myXPConfig)
                , ((modMask,                 xK_z), warpToWindow 1 1)
                , ((modMask,                 xK_q), recompile True >> restart "xmonad" True)
                , ((modMask .|. controlMask, xK_l), spawn "exec xlogo -render -fg `randomdarkcolor` -bg `randomdarkcolor`") -- For testing layouts.
                ]
