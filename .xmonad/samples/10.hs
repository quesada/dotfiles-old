-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Andrea_Spada_xmonad.hs

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W 
import Data.Bits ((.|.))
import System.Exit
import System.IO
import qualified Data.Map as M

import XMonad.Layout.TwoPane
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Submap


main = xmonad $ defaultConfig
                { borderWidth		= 1
                , focusedBorderColor 	= "#ff6666"
                , normalBorderColor 	= "#2222aa"
                , manageHook    	= myManageHook <+> manageDocks
                , workspaces    	= map show [1 .. 10 :: Int]
                , terminal		= "urxvtc -tr -sh 30"
		, modMask       	= mod4Mask
                , logHook       	= myLogHook
                , layoutHook    	= windowNavigation $ (avoidStruts (tall ||| Mirror tall ||| mosaic ||| combo ||| Full))
                , keys 			= \c -> myKeys c `M.union` keys defaultConfig c
                }
                
	where 
	
	tall 	= Tall 1 (3/100) (488/792)
	mosaic 	= MosaicAlt M.empty
	combo 	= combineTwo (TwoPane 0.03 (3/10)) (mosaic) (Full)
        
        myLogHook :: X ()
        myLogHook = do ewmhDesktopsLogHook
                       return ()

	myManageHook :: ManageHook
	myManageHook = composeAll . concat $
	    	[ [ title =? "xclock"	-->  doFloat ]
		, [ className =? "Qjackctl"	--> doFloat ]
		, [ className =? "Gimp"		--> doFloat ] 	
	    	]
       
        myKeys (XConfig {modMask = modm}) = M.fromList $

		-- Apps and tools
		[ ((modm, xK_c), spawn "gmrun")
		, ((modm, xK_m), spawn "claws-mail")
		, ((modm, xK_f), spawn "firefox")
		, ((modm, xK_n), spawn "nautilus")
		, ((modm, xK_r), spawn "rox")
		, ((modm, xK_s), spawn "skype")
		, ((modm .|. shiftMask, xK_s), spawn "gnome-search-tool")
		, ((modm, xK_End), spawn "gnome-session-save --kill")

		-- Special commands
		, ((modm, xK_F11 ),  spawn "xlock")
		, ((modm, xK_F12 ),  spawn "gdmflexiserver")
		, ((modm, xK_Print), spawn "scrot '/tmp/%Y-%m-%d_%H:%M:%S_$wx$h_scrot.png' -e 'mv $f ~'")
		
		-- Layout bindings
		-- 
		-- Mosaic 
		, ((modm, xK_KP_Add), withFocused (sendMessage . expandWindowAlt))
		, ((modm, xK_KP_Subtract), withFocused (sendMessage . shrinkWindowAlt))
		, ((modm .|. controlMask, xK_space), sendMessage resetAlt)

		-- Window Navigation
		-- select...
		, ((modm, xK_Right), sendMessage $ Go R)
		, ((modm, xK_Left ), sendMessage $ Go L)
		, ((modm, xK_Up   ), sendMessage $ Go U)
		, ((modm, xK_Down ), sendMessage $ Go D)

		-- swap...
		, ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
		, ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
		, ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
		, ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)

		-- move...
		, ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
		, ((modm .|. controlMask .|. shiftMask, xK_Left), sendMessage $ Move L)
		, ((modm .|. controlMask .|. shiftMask, xK_Up), sendMessage $ Move U)
		, ((modm .|. controlMask .|. shiftMask, xK_Down), sendMessage $ Move D)

		]


