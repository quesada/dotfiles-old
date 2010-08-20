-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/lazor%27s_xmonad.hs
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Layout.Dishes
import XMonad.Layout.MosaicAlt

myKeys (XConfig {modMask = modm}) = M.fromList $
    [ ((modm, xK_p), spawn "krunner")
    , ((modm .|. shiftMask, xK_q), spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:0 int32:1")
    , ((modm, xK_a), withFocused (sendMessage . expandWindowAlt))
    , ((modm, xK_y), withFocused (sendMessage . shrinkWindowAlt))
    , ((modm, xK_s), withFocused (sendMessage . tallWindowAlt))
    , ((modm, xK_d), withFocused (sendMessage . wideWindowAlt))
    , ((modm .|. controlMask, xK_space), sendMessage resetAlt)
    , ((mod1Mask, xK_Tab), windows W.focusDown)
    , ((mod1Mask .|. shiftMask, xK_Tab), windows W.focusUp)
    ]

myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat | c <- myFloats]
    , [ title =? t --> doFloat | t <- myOtherFloats]
    , [ className =? c --> doIgnore | c <- myIgnores]
    , [ title =? t --> doIgnore | t <- myOtherFloats]
    --, [ resource =? "desktop_window" --> doIgnore ]
    ] where

        myFloats = ["Plasma"]
        myOtherFloats = []
        myIgnores = [] --["Qt-subapplication","Plasma"]
        myOtherIgnores = []

myLayout = mosaic ||| Mirror mosaic ||| Full
  where
    mosaic = MosaicAlt M.empty
    dishes = Dishes 2 (1/6)
    tiled   = Tall 1 (3/100) (1/2)

myWorkspaces = ["1","2","3","4","5","6"]

myNormalBorderColor  = "#339900"
myFocusedBorderColor = "#66ff66"
myBorderWidth = 3

main = xmonad $ desktopConfig
 { terminal = "konsole"
 , keys     = \c -> myKeys c `M.union` keys desktopConfig c
 , modMask = mod4Mask -- use the Windows button as mod
 , logHook = ewmhDesktopsLogHook >> setWMName "LG3D"
 , manageHook = manageHook desktopConfig <+> myManageHook
 , layoutHook = ewmhDesktopsLayout $ avoidStruts $ desktopLayoutModifiers myLayout
 , workspaces = myWorkspaces
 , normalBorderColor = myNormalBorderColor
 , focusedBorderColor = myFocusedBorderColor
 , borderWidth = myBorderWidth
 }
