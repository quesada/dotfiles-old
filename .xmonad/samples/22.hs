-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Gwern%27s_xmonad.hs
import Data.Bits (Bits((.|.)))
import Data.Map as M (fromList, Map())
import XMonad
import XMonad.Actions.GridSelect (defaultGSConfig, goToSelected)
import XMonad.Actions.Search (google, isohunt, wayback, wikipedia, wiktionary, selectSearch, promptSearch)
import XMonad.Actions.WindowGo (raiseMaybe, raiseBrowser, raiseEditor, runOrRaise)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, FocusHook(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt (greenXPConfig)
import XMonad.Prompt.Shell (shellPrompt, prompt, safePrompt)
import XMonad.StackSet as W (focusUp, focusDown, sink)
import XMonad.Util.Run (unsafeSpawn, runInTerm, safeSpawnProg)
import XMonad.Util.XSelection (safePromptSelection)
import XMonad.Hooks.SetWMName (setWMName)

main :: IO ()
main = spawn "emacs --daemon" >> xmonad myConfig
 where myConfig = withUrgencyHook FocusHook $ gnomeConfig { focusedBorderColor = "red"
                         , keys = \c -> myKeys c <+> keys defaultConfig c
                         , layoutHook =  avoidStruts $ smartBorders (Full ||| Mirror tiled ||| tiled)
                         , logHook    = ewmhDesktopsLogHook >> setWMName "LG3D"
                         , manageHook = myManageHook
                         , modMask = mod4Mask
                         , normalBorderColor  = "grey"
                         , terminal = "urxvt"
                         , XMonad.workspaces = ["web", "irc", "code", "4"] }
           where tiled = Tall 1 0.03 0.5

{- Important things to note: We specifically don't use 'managehook
   defaultConfig, since I don't like floating mplayer and I don't use the other
   specified applications. Otherwise, we have manageDocks there to allow use of
   gnome-panel; Firefox/Emacs/Irssi go to their designated workspaces. -}
myManageHook :: ManageHook
myManageHook = composeAll [moveT "Amphetype" "code",
                           moveT "Brain Workshop 4.7" "code",
                           moveC "Emacs"     "code",
                           moveC "Navigator" "web",
                           moveC "Gimp"      "irc",
                           moveC "gscan2pdf" "code",
                           moveC "Mnemosyne" "code",
                           moveT "irssi"     "irc",
                           className =? "defcon.bin.x86" --> unfloat,
                           className =? "Darwinia" --> unfloat,
                           className =? "gnome-panel" --> doIgnore,
                           className =? "Mnemosyne" --> unfloat,
                           title     =? "Brain Workshop 4.7" --> unfloat]
                           <+> manageDocks
          where moveC c w = className =? c --> doShift w
                moveT t w = title     =? t --> doShift w
                unfloat = ask >>= doF . W.sink

myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {modMask = m, terminal = term}) = M.fromList [ -- rebind standard keys
            ((m .|. shiftMask,xK_p), shellPrompt greenXPConfig)
          , ((m,              xK_k), kill)
          , ((m,              xK_n), windows W.focusDown)
          , ((m,              xK_p), windows W.focusUp)
          , ((m,              xK_z), withFocused $ windows . W.sink) -- unfloat
          -- Custom bindings and commands
          , ((m,              xK_s), goToSelected defaultGSConfig)
          , ((m               ,xK_a), safeSpawnProg "/home/gwern/bin/bin/amphetype")
          , ((m,              xK_b), safePrompt "firefox" greenXPConfig)
          , ((m .|. shiftMask,xK_b), safePromptSelection "firefox")
          , ((m,              xK_c), safeSpawnProg term)
          , ((m .|. shiftMask,xK_c), prompt (term ++ " -e") greenXPConfig)
          , ((m .|. shiftMask,xK_d), raiseMaybe (runInTerm "-title elinks" "elinks") (title =? "elinks"))
          , ((m,              xK_e), raiseEditor)
          , ((m .|. shiftMask,xK_e), prompt "emacsclient -c -a emacs" greenXPConfig)
          , ((m,              xK_g), promptSearch greenXPConfig google)
          , ((m .|. shiftMask,xK_g), selectSearch google)
          , ((m,              xK_t), promptSearch greenXPConfig wikipedia)
          , ((m .|. shiftMask,xK_t), selectSearch wikipedia)
          , ((m,              xK_u), promptSearch greenXPConfig isohunt)
          , ((m .|. shiftMask,xK_u), selectSearch isohunt)
          , ((m,              xK_y), promptSearch greenXPConfig wayback)
          , ((m .|. shiftMask,xK_y), selectSearch wayback)
          , ((m .|. shiftMask,xK_w), selectSearch wiktionary)
          , ((m,              xK_w), safeSpawnProg "/home/gwern/bin/bin/brainworkshop")
          , ((m,          xK_Print), unsafeSpawn "import -quality 90 -window root png:$HOME/xwd-$(date +%s)$$.png")
          , ((m,              xK_i), raiseMaybe (runInTerm "-title irssi" "sh -c 'screen -r irssi'") (title =? "irssi"))
          , ((m,              xK_m), runOrRaise "mnemosyne" (className =? "Mnemosyne"))
          , ((m,              xK_r), raiseMaybe (runInTerm "-title rtorrent" "sh -c 'screen -r rtorrent'") (title =? "rtorrent"))
          , ((m,              xK_d), raiseBrowser) ]
