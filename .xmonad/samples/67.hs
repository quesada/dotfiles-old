-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/sereven%27s_xmonad.hs_one-host
------ sereven.0.8.base.xmonad.hs ----------------
import XMonad
import qualified XMonad.StackSet as W
import System.Environment (getEnvironment)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat, isFullscreen, doFullFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM -- (m%n) can be (m/n) don't need Data.Ratio
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare (getSortByTag,getSortByIndex)

modm = mod4Mask
sWorkspaces = [ [x] | x <- ['A'..'I'] ] -- /3 MINIMUM/ (easy to mod num wss)

ws2 = sWorkspaces !! 1
ws3 = sWorkspaces !! 2

main = do
    dz <- spawnPipe $ dzenWith darkXPC
    home <- fmap (maybe "/home/svv/" (++"/") . lookup "HOME") getEnvironment
    conf <- withWindowNavigation (xK_k, xK_h, xK_j, xK_l) $
            withUrgencyHook NoUrgencyHook $
               defaultConfig
                  { modMask            = modm
                  , focusFollowsMouse  = True
                  , terminal           = "urxvtc"
                  , focusedBorderColor = "#694"
                  , normalBorderColor  = "black"
                  , borderWidth        = 1
                  , workspaces         = sWorkspaces
                  , layoutHook         = sLayouts
                  , logHook            = dynamicLogWithPP $ dzdarkPP home dz
                  , manageHook         = sManageHook
                  }  `additionalKeys` sKeys `additionalKeysP` sKeysP
                     -- silly to use both, but.. meh..
    xmonad conf

sKeys = -- need normal bindings for testing ScreenWorkspaces
    [ ((modm .|. controlMask, xK_space), sendMessage TL.ToggleLayout )
    , ((modm,xK_b)    , sendMessage ToggleStruts)
    , ((shiftMask,xK_F1), shellPrompt darkXPC)
    , ((shiftMask,xK_F2), runOrRaisePrompt darkXPC)
    , ((shiftMask,xK_F3), scratchpadSpawnActionTerminal "urxvt -pe tabbed")

    -- for more screens, workspaces, or exploring ion style subframes
    , ((modm,               xK_backslash), selectWorkspace darkXPC)
    , ((modm .|. shiftMask ,xK_BackSpace), removeWorkspace)
    , ((modm               ,   xK_F8), rescreen)
    , ((modm  .|. shiftMask,   xK_F8), layoutScreens 4 (Tall 2 (1/8) (1/2)))
    ]

sKeysP = -- TODO convert to additionalKeys
    [ ("M-C-k", sendMessage MirrorExpand)
    , ("M-C-j", sendMessage MirrorShrink)
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)
    ]
    ++ -- workspace and screen keys ------------------------------------
       -- mod-<key> and mod-shift-<key> are as default
       -- mod-ctrl-<key> shifts focused window *and* view to <key>'s ws
    [ (addtlmods ++ "M-" ++ [key], action tag)
    |   (tag, key)  <- zip sWorkspaces "1234567890_="
      , (addtlmods, action) <- [ ("", windows . W.greedyView) -- or W.view)
                               , ("S-", windows . W.shift)
                               , ("C-", \x -> (windows . W.shift) x >> (windows . W.view) x)]
    ]

sManageHook = composeAll
    [ scratchpadManageHookDefault
    , isFullscreen              --> doFullFloat
    , className =? "Gimp"       --> doShift ws3 -- don't float
    , className =? "MPlayer"    --> doShift ws2
    , title =? "shellfm"        --> doShift ws2

    , title =? ""               --> doFloat     -- SOE graphics
    , title =? "shellfm"        --> doFloat
    , className =? "MPlayer"    --> doFloat
    , className =? "Hback"      --> doCenterFloat
    , className =? "XFontSel"   --> doCenterFloat
    , className =? "Xmessage"   --> doCenterFloat
    ]

sLayouts =
    smartBorders . avoidStruts $
    TL.toggleLayouts (noBorders Full) $
    modWorkspace ws3 reflectHoriz $ withIM (11/64) (Role "gimp-toolbox") $
    onWorkspace  ws2 (TwoPane (1/118) (11/15)) $
    ResizableTall 2         -- default number of masters
                  (1/118)   -- resize increment
                  (11/20)   -- horizontal ratio
                  [5/4 -- master column ~ top/bottom
                  ,5/4 -- no effect w/ 2 masters
                  ,5/4 -- slave  column ~ top/bottom
                  ]    -- then defaults to (repeat 1)

-- (theming, font, colors) ------------------
darkXPC :: XPConfig
darkXPC = defaultXPConfig
      { font = "-*-dejavu sans mono-medium-r-*-*-17-*-*-*-*-*-*-*"
      , height   = 28
      , bgColor  = "black"
      , fgColor  = "#684"
      , bgHLight = "#785"
      , fgHLight = "black"
      , promptBorderWidth = 0
      }

-- (dzen) ----------------------------------
dzenWith :: XPConfig -> String
dzenWith xpc = -- use XPConfig theming
    "dzen2 -xs 1 -ta l -h " ++ (show $ height xpc)
      ++ " -fn '" ++ font    xpc ++ "'"
      ++ " -bg '" ++ bgColor xpc ++ "'"
      ++ " -fg '" ++ fgColor xpc ++ "'"
      ++ " -e 'onstart=lower'"

dzdarkPP  hm dzIn = defaultPP
    { ppOutput          = hPutStrLn dzIn
    , ppCurrent         = wrap (dzfg "#4d5" box) ""  . dzfg "#bca"
    , ppVisible         = dzfg "#ab9" . (emptybox ++)
    , ppHidden          = dzfg "#786"
    , ppHiddenNoWindows = dzfg "#554"
    , ppUrgent          = dzfg "#d54" . stripDzen
    , ppWsSep           = kernedsp
    , ppSep             = ""
    , ppSort            = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
    , ppExtras          = [xmonicon]
    , ppOrder           = \(ws:_:_:xs) ->  ["    "] ++ [ws] ++ xs
    }
    -- for layouts and titles use something like ppOrder = \(ws:l:t:xs) ->  [l,ws,t] ++ xs
  where dzfg c      = dzenColor c ""
        box = "^p(;+7)^r(5x5)^p(+2;-7)"
        emptybox = "^p(;+7)^ro(5x5)^p(+2;-7)"
        kernedsp    = "^p(+12)"
        xpm path = wrap ("^i(" ++ path ++ ".dzen/icons/") ".xpm)"
        xmonicon = io $ return . Just $
            kernedsp ++ "^p(;4)" ++ xpm hm "xmonad16" ++ "^p(;-4)" ++ kernedsp

stripDzen :: String -> String -- strip dzen formatting to undo ppHidden
stripDzen = strip [] where
    strip keep [] = keep
    strip keep ('^':'^':x) = strip (keep ++ "^") x
    strip keep ('^':x) = strip keep (drop 1 . dropWhile (')' /=) $ x)
    strip keep x = let (good,x') = span ('^' /=) x
        in strip (keep ++ good) x'
