-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Andrea_Rossato%27s_xmonad.hs
module Main (main) where

import XMonad
import XMonad.Config.Arossato (arossatoConfig)

main :: IO ()
main = xmonad =<< arossatoConfig
