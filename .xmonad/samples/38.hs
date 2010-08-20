-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/sereven-xmonad.hs
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
-- ~/.xmonad/xmonad.hs (0.9) $ 2009-10-04

-- imports  {{{
import XMonad
import qualified XMonad.StackSet as W

-- standard libraries --
import Prelude hiding (mod)
import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.List (isInfixOf,nub)

-- xmonad-contrib-darcs --
import XMonad.Actions.CycleWS (swapNextScreen, toggleWS, toggleOrDoSkip)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM
import XMonad.Layout.LayoutScreens
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare (getSortByIndex)

-- ~/.xmonad/lib/ --
-- (~>) is infix (,)
import ExtraCombinators ((~>)) -- http://code.haskell.org/~wwolff/_xmonad/lib/ExtraCombinators.hs -- thanks mauke!
import XMonad.Layout.TallAlt   -- http://www.haskell.org/pipermail/xmonad/2009-July/008270.html   -- thanks aavogt!

-- }}}

-- main {{{

home         = ("/home/me/" ++)
xmpath       = id -- xmpath = (home ".cabal/bin/" ++) -- (home "tmp/bin/" ++)
promptConfig = darkXPConfig
wsIds = map return "ASDQWE123" ++ ["NSP"] -- bound on these qwerty keys

main = do
    dz <- spawnPipe $ toDzenCmd darkXPConfig
    conf <- withWindowNavigation vi . withUnfocusedUrgents $
        defaultConfig
            { terminal           = "urxvt"
            , modMask            = mod4Mask
            , focusedBorderColor = fgColor promptConfig
            , normalBorderColor  = bgColor promptConfig
            , workspaces         = wsIds
            , logHook            = onAllEvents dz
            , manageHook         = onWindowCreation
            , layoutHook         = layouts
            }
        `additionalKeysP` keybindings `additionalMouseBindings` mousebindings
    xmonad conf { startupHook = do
                    return () >> checkKeymap conf keybindings
                    spawnOnce apps
                    }
 where
    vi = (xK_k, xK_h, xK_j, xK_l)
    -- visible urgents not really displayed nicely, more for testing at the moment:
    withUnfocusedUrgents = withUrgencyHookC NoUrgencyHook urgencyConfig {suppressWhen = Focused}
    apps = [ (home ".cabal/bin/xmobar", home ".xmobarrc-1-top")
           , ("xcompmgr", "-c -r 3 -l -5 -t -5")
           ]
    onAllEvents h = do
        dynamicLogWithPP dzdarkPP { ppOutput = hPutStrLn h }
        fadeMostInactives 0.9375 -- ~0xefffffff
        updatePointer (Relative 0.8 0.96)

-- # helpers to move to lib/ or contrib
spawnOnce :: (MonadIO m) => [(String, String)] -> m ()
spawnOnce = mapM_ $ uncurry sp
  where sp cmd rest = spawn $ concat ["[ ! $(pidof ", cmd, ") ] && ", cmd, " ", rest]

fadeMostInactives :: Rational -> X ()
fadeMostInactives = fadeOutLogHook . fadeIf (isUnfocused <&&> noneOf qs)
  where noneOf = fmap not . foldr1 (<||>)
        qs = [isFullscreen, fmap ("layer" `isInfixOf`) className, className =? "Cinelerra", className =? "Gimp"]

-- }}}

-- bindings {{{
keybindings =
       maskedBy nix
    [ "<Tab>"       ~> scratchpadSpawnActionTerminal "urxvt -pe tabbed"
    , "<Space>"     ~> sendMessage ToggleLayout  -- fullscreen
    , "\\"          ~> sendMessage NextLayout
--  , "t"           ~> sendMessage ToggleStruts
    , "<F12>"       ~> clearUrgents
    , "<F8>"        ~> rescreen                          -- two screens:   left, right
    , "<F9>"        ~> layoutScreens 3 (Tall 2 0.5 0.5)  -- three screens: left, right_top, right_bottom
    ]
    ++ maskedBy mod
    [ "<Space>"     ~> sendMessage ToggleLayout  -- fullscreen
    , "\\"          ~> sendMessage NextLayout
    , "<Return>"    ~> windows W.shiftMaster
    , "="           ~> windows $ W.greedyView "NSP"
    , "q"           ~> spawn $ xmpath "xmonad --recompile; " ++ xmpath "xmonad --restart"
    ]
    ++ maskedBy (control . mod)
    [ "h"           ~> sendMessage Shrink
    , "l"           ~> sendMessage Expand
    , "k"           ~> windows W.swapUp
    , "j"           ~> windows W.swapDown
    ]
    -- searches
    ++ maskedBy win
    ["/ " ++ ks ~> promptSearch promptConfig s | (ks,s) <- searches]
    ++ maskedBy (control . win)
    ["/ " ++ ks ~> selectSearch s | (ks,s) <- searches]
    -- displays
    ++ maskedBy nix workspacesAndScreens
  where
    workspacesAndScreens = -- 1 2 3 \ 4   \ backspace
                           --  q w e \ r
                           --   a s d \ f g -- screens
                           --          \ v b
        [ othermasks ++ [key] ~> action i
            | (i, key)  <- zip wsIds "asdqwe123="
            , (othermasks, action) <- [ ("", toggled W.greedyView)
                                      , ("S-", toggled W.shift)
                                      , ("S-C-", toggled followShift) ] ]
        ++
        [ "4"           ~> toggleWS
        , "v"           ~> swapNextScreen
        , "<Backspace>" ~> removeWorkspace
        ]
        ++
        [ othermasks ++ [key] ~> screenWorkspace i >>= flip whenJust (windows . action)
            | (i, key)  <- zip [0..] "fgb"
            , (othermasks, action) <- [ ("", W.view) , ("S-", W.shift)] ]
        --
    toggled = toggleOrDoSkip ["NSP"]
    followShift = liftM2 (.) W.view W.shift
        --
    hayoo       = searchEngine "Hayoo"       "http://holumbus.fh-wedel.de/hayoo/hayoo.html?query="
    hpaste      = searchEngine "hpaste"      "http://hpaste.org/fastcgi/hpaste.fcgi/?search="
    gmane_beg   = searchEngine "beg"         "http://search.gmane.org/?group=gmane.comp.lang.haskell.beginners&query="
    gmane_cafe  = searchEngine "cafe"        "http://search.gmane.org/?group=gmane.comp.lang.haskell.cafe&query="
    gmane_sup   = searchEngine "sup"         "http://search.gmane.org/?group=gmane.mail.sup.general&query="
    gmane_xm    = searchEngine "xm"          "http://search.gmane.org/?group=gmane.comp.lang.haskell.xmonad&query="
    bugsall_xm  = searchEngine "bugsAll_xm"  "http://code.google.com/p/xmonad/issues/list?can=1&q="
    bugsopen_xm = searchEngine "bugsOpen_xm" "http://code.google.com/p/xmonad/issues/list?can=2&q="
    bugsnew_xm  = searchEngine "bugsNew_xm"  "http://code.google.com/p/xmonad/issues/list?can=6&q="
    searches    = [ ("g"  , google)
                  , ("h"  , hoogle)
                  , ("S-h", hayoo)
                  , ("p"  , hpaste)
                  , ("w"  , wikipedia)
                  , ("b"  , gmane_beg)
                  , ("c"  , gmane_cafe)
                  , ("s"  , gmane_sup)
                  , ("x x", gmane_xm)
                  , ("x a", bugsall_xm)
                  , ("x o", bugsopen_xm)
                  , ("x n", bugsnew_xm)
                  ]
    -- excessively cute helpers
    maskedBy = map . first
    [_,control] = let ezstr c = (++) (c : "-") in map ezstr "SC"
    [_,_,nix,win,_] = map ((++) . ezstring) [1..5] :: [String -> String]
    mod = ( "M-"  ++)
    ezstring :: Int -> String -- default would have been Integer
    ezstring n | n `elem` [1..5] = 'M' : (show n ++ "-")
               | otherwise       = "M-"

mousebindings = -- FlexibleManipulate rulez! -- change mod3Mask to your mod:
                -- If use IM Full try these mouse wheel bindings over gimp toolbox to
                -- select main window, prevents nasty switching.
    [ (mod3Mask, button3) ~> Flex.mouseWindow Flex.discrete
    , (mod3Mask, button4) ~> const $ windows W.swapDown
    , (mod3Mask, button5) ~> const $ windows W.swapUp ]
    -- }}}

-- window hooks {{{
onWindowCreation =  composeAll
    [ scratchpadManageHook (W.RationalRect 0.325 0.6 0.641 0.35)
    , title        =? ""                     --> doFloat
    , title        =? "x"                    --> doFloat -- weird cinelerra splash window
    , className    =? "Cinelerra"            --> doFloat
    , className    =? "Xloadimage"           --> doCenterFloat
    , className    =? "XFontSel"             --> doCenterFloat
    , className    =? "Xmessage"             --> doCenterFloat
    , className    =? "Hback"                --> doCenterFloat
    , fmap ("Gimp" `isPrefixOf`) className   --> doShift "W"
    , role         =? "conversation"         --> doShift "D"
    , title        =? "Buddy List"           --> doShift "NSP"
    , fmap ("layer" `isInfixOf`) className   --> doF W.focusDown <+> doFloat -- MP.layer|Gnome-[Mm]p.layer
    , isFullscreen                           --> doF W.focusDown <+> doFullFloat
    , isDialog                               --> doFloat
    , transience'
    , manageDocks
    ]
  where role = stringProperty "WM_WINDOW_ROLE"

layouts = smartBorders . toggleLayouts (noBorders Full) . avoidStruts $
          onWorkspaces ["D","E","3"] moreWindows . onWorkspace "W" gimp $ defaults
  where
    defaults = mirror4 ||| moreWindows ||| Full
    mirror4 = limitWindows 4 (Mirror $ TallAlt (1/vpixels) (817/1182))
    moreWindows = TallAlt (1/800) (722/1325)
    gimp = reflectHoriz $ withIM 0.16 (Role "gimp-toolbox") (mirror4 ||| Full)
    vpixels = fromIntegral $ 1200 - height promptConfig - 3
-- }}}

-- prompt and dzen {{{ -- hideousity creeping slowly toward beauty
toDzenCmd xpc =
    "dzen2 -xs 1"
      ++ " -h "  ++ show (height xpc)
      -- with xft use:
      ++ " -fn " ++ "'" ++ drop 4 (font xpc) ++ "'"
      -- with core fonts use:
--    ++ " -fn " ++ "'" ++ font xpc ++ "'"
      ++ " -bg " ++ "'" ++ bgColor xpc ++ "'"
      ++ " -fg " ++ "'" ++ fgColor xpc ++ "'"
      ++ " -ta l"
      ++ " -e 'onstart=lower'"

darkXPConfig = defaultXPConfig
    { font = "xft:Nimbus Sans L Bold Condensed:size=10"
    , height   = 15 -- 18 px line height - 3 border px
    , bgColor  = "gray14"
    , fgColor  = "cornsilk4"
    , bgHLight = "#785"
    , fgHLight = "black"
    , promptBorderWidth = 0
    , showCompletionOnTab = True
    , historySize = 12
    , historyFilter = nub
    }

-- PP {{{
dzdarkPP  = defaultPP
    -- ppOutput defined in main
    { ppUrgent          = dzenColor "black" urgentCol
    , ppCurrent         = dzfg hlightCol  . (box 4 ++) . dzfg currentCol
    , ppVisible         = dzfg visibleCol . (obox 4 ++)
    , ppHiddenNoWindows = dzfg fadedCol
    , ppHidden          = dzfg hiddenCol
    , ppWsSep           = "^p(+12)"
    , ppSep             = ""
    , ppSort            = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
    , ppOrder           = \(ws:_:t:_) ->  ["    ", ws, "         ^p(;-1)", t] -- \(ws:l:t:xs) -> (?strs :: [String])
    }
  where dzfg fgc   = dzenColor fgc ""
        box, obox :: Int -> String
        box      y = concat ["^p(;+", show y, ")^r(5x5)^p(+2;-", show y,")"]
        obox     y = concat ["^p(;+", show y, ")^ro(5x5)^p(+2;-", show y, ")"]
        hlightCol  = "#4d5"
        currentCol = "#bca"
        visibleCol = "#ab9"
        fadedCol   = "#554"
        urgentCol  = "#d54"
        hiddenCol  = "#786"
        -- }}}

-- }}}

-- vim:foldmethod=marker
