-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/TeXitoi%27s_xmonad.hs
import XMonad hiding (Tall)
import XMonad.Config.Desktop
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.HintedTile
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = xmonad desktopConfig {
    modMask = mod4Mask,
    terminal = "urxvtc",
    keys = \c -> myKeys c `M.union` keys desktopConfig c,
    startupHook = startupHook desktopConfig >> setWMName "LG3D",
    logHook = logHook desktopConfig >> fadeInactiveLogHook 0xcccccccc,
    layoutHook = desktopLayoutModifiers . smartBorders $ tiled ||| Full,
    manageHook = myManageHook <+> manageHook desktopConfig,
    workspaces = map show [1..8],
    focusedBorderColor = "#729fcf",
    normalBorderColor = "#aaaaaa"
}

tiled = HintedTile 1 0.03 0.5 TopLeft Tall

myKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [((modm, xK_semicolon), sendMessage (IncMasterN (-1))),
     ((mod1Mask, xK_Tab), windows W.focusDown),
     ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp),
     ((mod4Mask, xK_i), sendMessage NextLayout)] ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) numBepo,
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

numBepo = [0x22,0xab,0xbb,0x28,0x29,0x40,0x2b,0x2d,0x2f,0x2a]
numAzerty = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]

myManageHook = composeAll [className =? c --> doShift w | (w, cs) <- wcs, c <- cs]
    where wcs = [("2", ["Midori", "Firefox"]),
                 ("3", ["Gajim.py"]),
                 ("4", ["Claws-mail"]),
                 ("5", ["Gmpc"])]
