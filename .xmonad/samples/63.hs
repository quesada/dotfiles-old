-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/OldSchoolWSNav_xmonad.hs
-- Using XMonad's (|||) now. Uncomment its hiding, and instead
-- comment LayoutCombinator's hiding to try the JumpToLayout stuff
import XMonad -- hiding ( (|||) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders(smartBorders)
import XMonad.Layout.Tabbed(simpleTabbed)
import XMonad.Util.Run(spawnPipe,hPutStrLn)
import XMonad.Util.EZConfig(additionalKeysP)

-- for old school ws nav, tying screens together
import XMonad.Layout.LayoutScreens(layoutScreens,fixedLayout)
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
-- LayoutCombinators.(*|*) is weird with mod-, and mod-.                                                                                                       
-- had hoped it might make (simpleTabbed *|* simpleTabbed) work, but no joy.
import XMonad.Layout.Named(named)
import XMonad.Layout.Reflect(reflectHoriz)

fnt = "-*-fixed-*-r-*-15-*-*-*-*-*-iso8859-1"
bgc = "black"
fgc = "grey"
dzArgs = concat [" -fn '",fnt,"'"," -bg '",bgc,"' -fg '",fgc,"'"," -ta l -h 20 "," -w 1600 -e 'onstart=lower'"]

stdLayoutHook = smartBorders . avoidStruts  $  layoutHook defaultConfig

-- | Use namedLayoutHook with xmonad's (|||) hidden, and LayoutCombinator's (|||) exposed.
-- | Only Tall will then be available from layoutHook defaultConfig, as written now.
-- | To use for real, rewrite `Tall ...||| Mirror Tall ... ||| Full' in xmonad.hs
namedLayoutHook = smartBorders . avoidStruts $
    named "Defaults:Full" (layoutHook defaultConfig *|* Full) |||
    named "Full:Defaults" (reflectHoriz $ layoutHook defaultConfig *|* Full) |||
    named "Tabbed:Full" (simpleTabbed *|* Full) |||
    named "Full:Tabbed" (reflectHoriz $ simpleTabbed *|* Full)

-- | This layout, using XMonad's (|||), accesses all combinations with mod-space
myLayoutHook = smartBorders . avoidStruts $
    (layoutHook defaultConfig *|* Full) |||
    (reflectHoriz $ layoutHook defaultConfig *|* Full) |||
    (simpleTabbed *|* Full) |||
    (reflectHoriz $ simpleTabbed *|* Full)


main = do
  h <- spawnPipe $ "dzen2" ++ dzArgs
  xmonad $ defaultConfig
         { manageHook = manageDocks <+> manageHook defaultConfig
--       , layoutHook = stdLayoutHook   -- strange with LayoutCombinators.(|||)
--       , layoutHook = namedLayoutHook -- JumpToLayout/named example only
         , layoutHook = myLayoutHook
         , modMask    = mod4Mask        -- Rebind Mod to the Windows key
         , terminal   = "urxvt"
         , logHook = dynamicLogWithPP defaultPP { ppTitle  = shorten 50
                                                , ppOutput = hPutStrLn h
                                                }
         }
         `additionalKeysP`
         [ ("M-b"    , sendMessage ToggleStruts)
           -- edit Rectangle to match your screen dimensions
         , ("M-<F4>" , layoutScreens 1 (fixedLayout [Rectangle 0 0 3200 1200])) -- link screens
         , ("M-<F3>" , rescreen) -- return to xmonad defaults before mod-q into non LayoutScreens config

-- LayoutScreens JumpToLayout examples: requires hiding XMonad's, using LayoutScreen's (|||)
{-
           -- see namedLayoutHook comment for setup
         , ("M-<R>"    , sendMessage $ JumpToLayout "Defaults:Full")
         , ("M-<L>"    , sendMessage $ JumpToLayout "Full:Defaults")
         , ("M-C-<R>"  , sendMessage $ JumpToLayout "Tabbed:Full")
         , ("M-C-<L>"  , sendMessage $ JumpToLayout "Full:Tabbed")
 -}
{-
           -- use with myLayoutHook, not using named
         , ("M-<R>"    , sendMessage $ JumpToLayout "combining Tall and Full with Tall")
         , ("M-<L>"    , sendMessage $ JumpToLayout "ReflectX combining Tall and Full with Tall")
         , ("M-C-<R>"  , sendMessage $ JumpToLayout "combining Tabbed Simplest and Full with Tall")
         , ("M-C-<L>"  , sendMessage $ JumpToLayout "ReflectX combining Tabbed Simplest and Full with Tall")
 -}
         ]
