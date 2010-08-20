-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/sereven%27s_xmonad.hs
-- cross-host xmonad-0.8 xmonad.hs
--
-- Disclaimer: this is hs equivalent of BASIC goto spaghetti, hopefully by 0.9 I'll
-- post a better example, but at the time posted people were asking for dispatch
-- on host example, and no one else posted anything, so shamelessly shared.
--
------ extra cross host stuff most won't need   --
import System.Posix.Unistd (getSystemID, nodeName)
import System.Environment (getEnvironment)
------ also may not want scaling dzen bar --------
-- TODO: just use XMonad fns, use Rectangles
import Graphics.X11.Xlib
import Graphics.X11.Xinerama
------ normal imports                           --
import XMonad
import qualified XMonad.StackSet as W
------                                          --
import Data.List (elem)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%)) -- for Layout.IM
------                                          --
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers(doCenterFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
------                                          --
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Actions.Warp
------                                          --
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.ToggleLayouts as TL
------                                          --
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.RunOrRaise
------                                          --
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig


-- "Make it so, Mr. X!" --------------------

main = do
    host   <- fmap  nodeName getSystemID
    home   <- fmap (fromMaybe "/home/me" . lookup "HOME") getEnvironment
    scrxpw <- getXPositionAndWidthOfScreen 0 -- dzen screen number
    let dzencmd = uncurry dzenWithParamsFrom (fromMaybe (0, 1024) scrxpw) $ xpcByHost host
    dz <- spawnPipe dzencmd

    -- uncomment to show dzen params while setting up other bars in .xinitrc
    -- spawn $ "xmessage " ++ "'" ++ dzencmd ++ "'"

    let conf  = confByHost host home dz
    let sKeys = keysByHost host home conf

    xmonad $ withUrgencyHook NoUrgencyHook $
      conf {startupHook = return () >> checkKeymap conf sKeys}
      `additionalKeysP` sKeys

    -- using this XConfig

baseConf = defaultConfig
    { borderWidth       = 1
    , workspaces        = sWorkspaces
    , manageHook        = sManageHook
    , focusFollowsMouse = True
    , terminal          = "urxvtc"
    }

confByHost h hm dzh
  | isLightHost  h = baseConf
      { layoutHook         = layoutByHost h
      , logHook            = dynamicLogWithPP $ dzlightPP hm dzh
      , focusedBorderColor = dkCol ltCols
      , normalBorderColor  = ltCol ltCols
      , modMask            = mod3Mask
      }
  | otherwise   = baseConf 
      { layoutHook         = layoutByHost h
      , logHook            = dynamicLogWithPP $ dzdarkPP hm dzh
      , focusedBorderColor = ltCol dkCols
      , normalBorderColor  = dkCol dkCols
      , modMask            = mod4Mask
      }

-- (frequently edited bits nearer the top) -------

isLightHost :: String -> Bool
isLightHost hostname = -- hosts with light bg, small screen, no winkey configs
    hostname `elem` ["azim", "maraire", "NS41266"]

keysByHost :: String -> String -> XConfig l -> [(String, X())]
keysByHost hst hm cfg =

    [ ("M-<F1>",  shellPrompt $ xpcByHost hst)
    , ("M-<F2>",  runOrRaisePrompt $ xpcByHost hst)
    , ("M-<F3>",  scratchpadSpawnAction cfg)
    , ("M-<F12>", spawn "xscreensaver-command -lock")

    , ("M-x f", runOrRaise "firefox" (className =? "Firefox"))
    , ("M-x o", runOrRaise "opera" (className =? "Opera"))
    , ("M-x r", runOrRaise (hm ++ "/bin/shfmRun") (title =? "shellfm"))
    , ("M-x h", runOrRaise "hback" (className =? "Hback"))
    , ("M-x g", spawn "gimp")
    ]
    ++ -- vi directional window navigation similar to XMonad defaults
    [ ("M-k", sendMessage $ Go U)
    , ("M-j", sendMessage $ Go D)
    , ("M-h", sendMessage $ Go L)
    , ("M-l", sendMessage $ Go R)
    , ("M-S-k", sendMessage $ Swap U)     -- shift shifts windows
    , ("M-S-j", sendMessage $ Swap D)
    , ("M-S-h", sendMessage $ Swap L)
    , ("M-S-l", sendMessage $ Swap R)
    , ("M-C-k", sendMessage MirrorExpand) -- ctrl controls size
    , ("M-C-j", sendMessage MirrorShrink)
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)
    ]
    ++ -- mod-<key> and mod-shift-<key> are as default (plus 0 and = too)
       -- mod-ctrl-<key> shifts focused window *and* view to <key>'s ws
    [ (addtlmods ++ "M-" ++ [key], action tag)
    |   (tag, key)  <- zip sWorkspaces "1234567890="
      , (addtlmods, action) <- [ ("", windows . W.greedyView)
                               , ("S-", windows . W.shift)
                               , ("C-", \x -> (windows . W.shift) x >> (windows . W.view) x)]
    ]
    ++ --  mod + m + {left arrow or 'a' key} does action with ws to "left"
    [ (m ++ "M-" ++ key, action) | key <- ["<L>","a"]
      , (m, action) <- [ ("", prevWS), ("S-", shiftToPrev), ("C-", shiftToPrev >> prevWS)]
    ]
    ++ -- mod + m + {right arrow or 'd' key} does action with ws to "right"
    [ (m ++ "M-" ++ key, action) | key  <- ["<R>","d"]
      , (m, action) <- [ ("", nextWS), ("S-", shiftToNext), ("C-", shiftToNext >> nextWS)]
    ]
    ++
    [ ("M-<Space>", sendMessage TL.ToggleLayout ) -- noBorders Full
    , ("M-b", sendMessage ToggleStruts)
    , ("M-C-b", warpToScreen 0 (1/2) 0) -- open XM dzen slave; exit mouse manually to collapse :(
    , ("M-S-b", warpToScreen 0 0 0)  -- open sys dzen slave; exit mouse manually to collapse :(
    , ("M-S-m", warpToWindow 1 1) -- banish mouse to lower right of focused window
    ]

sWorkspaces = ["'%", "%", "%'"] ++ ["'*", "*", "*'"] ++ ["c", "m", "#"] ++ ["@", "SP"]

sManageHook = composeAll
    [ doF avoidMaster
    , scratchpadManageHookDefault

    , title =? "shellfm"        --> doShift "'*"
    , className =? "Gimp"       --> doShift "*" -- don't float, IM layout
    , className =? "MPlayer"    --> doShift "*'"

    , title =? ""               --> doFloat     -- SOE graphics
    , title =? "shellfm"        --> doFloat
    , className =? "Hback"      --> doCenterFloat
    , className =? "MPlayer"    --> doFloat
    , className =? "XFontSel"   --> doCenterFloat
    , className =? "Xmessage"   --> doCenterFloat
    ]
  where avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
        avoidMaster = W.modify' $ \c -> case c of
            W.Stack t [] (r:rs) -> W.Stack t [r] rs
            otherwise           -> c

--------- !! SPLIT HERE !! ------------------
---- for xselection copy paste --------------

layoutByHost h =
    avoidStruts $ configurableNavigation noNavigateBorders $ 
    TL.toggleLayouts (noBorders Full) $ 
    modWorkspace "*" reflectHoriz $ withIM (11%64) (Role "gimp-toolbox") $ -- see [gimp]
    tileByHost h
  where tileByHost n
          | n == "azim" = ResizableTall 2 (1/118) (11/15) [9/16,9/16,9/16]
          | otherwise   = ResizableTall 2         -- default number of masters
                                        (1/118)   -- resize increment
                                        (11/20)   -- horizontal ratio: mstr/(mstr+slv)?
                                        [5/4 -- master column ~ top/bottom?
                                        ,5/4 -- no effect w/ 2 masters
                                        ,5/4 -- slave  column ~ top/bottom
                                        ]    -- then defaults to (repeat 1)

    -- [gimp] -- Combining gimp-toolbox and gimp-dock into one works
    -- well in IM layout roster column. Drag panes from gimp-dock or
    -- toolbox to top separator or bar of toolbox to add and re-arrange.


-- (theming, font, colors) ------------------

data Colorset = Colorset
    { ltCol       :: String 
    , dkCol       :: String
    , currentCol  :: String 
    , hlightCol   :: String --rotate 2π/3 urgentCol
    , visibleCol  :: String             
    , hiddenCol   :: String 
    , fadedCol    :: String
    , urgentCol   :: String 
    , urgentCol'  :: String --rotate 4π/3 urgentCol
    }

dkCols = Colorset 
    { ltCol       = "cornsilk3"         
    , dkCol       = "black" 
    , currentCol  = "#bca"
    , hlightCol   = "#4d5"
    , visibleCol  = "#ab9"
    , hiddenCol   = "#786"
    , fadedCol    = "#554"
    , urgentCol   = "#d54"
    , urgentCol'  = "#54d"
    }

ltCols = dkCols   
    { ltCol       = "#d7e19f"
    , dkCol       = "#230"
    , currentCol  = "black"
    , hlightCol   = "#0e3"
    , visibleCol  = "#443"
    , hiddenCol   = "#564"
    , fadedCol    = "#ab9"
    , urgentCol   = "#e30"
    , urgentCol'  = "#30e"
    }             

xpcByHost :: String -> XPConfig
xpcByHost h
  | isLightHost h = defaultXPConfig
      { font = "-*-dina-bold-r-*-*-*-*-*-*-*-*-*-*"
      , height   = 18
      , bgColor  = ltCol ltCols
      , fgColor  = dkCol ltCols
      , bgHLight = dkCol ltCols
      , fgHLight = ltCol ltCols
      , promptBorderWidth = 0
      }
  | otherwise   = defaultXPConfig
      { font = "-*-dejavu sans mono-medium-r-*-*-17-*-*-*-*-*-*-*"
      , height   = 22
      , bgColor  = dkCol dkCols
      , fgColor  = ltCol dkCols
      , bgHLight = ltCol dkCols
      , fgHLight = dkCol dkCols
      , promptBorderWidth = 0
      }


-- (dzen) ----------------------------------

getXPositionAndWidthOfScreen :: Int -> IO (Maybe (Int, Int))
getXPositionAndWidthOfScreen n = do
    d        <- openDisplay ""
    screens  <- getScreenInfo d
    let rn = screens!! min (abs n) (length screens - 1)
    case screens of
        []        -> return Nothing
        [r]       -> return $ Just (fromIntegral $ rect_x r , fromIntegral $ rect_width r)
        otherwise -> return $ Just (fromIntegral $ rect_x rn, fromIntegral $ rect_width rn)
        -- using rect_y or rect_height isn't needed on any XM hosts here, sorry


-- adjust dzen position & width to screen; use XPConfig theming
dzenWithParamsFrom :: Int -> Int -> XPConfig -> String
dzenWithParamsFrom sx sw xpc =
    "dzen2 -x "  ++ show xpos
      ++ " -w "  ++ show width
      ++ " -h "  ++ show (height xpc)
      ++ " -fn " ++ "'" ++ font    xpc ++ "'"
      ++ " -bg " ++ "'" ++ bgColor xpc ++ "'"
      ++ " -fg " ++ "'" ++ fgColor xpc ++ "'"
      ++ " -ta l" 
      -- normal dzen config --
      ++ " -e 'onstart=lower'"

  where xpos  = sx +      sw * 11 `div` 32        -- a`div`b screenwidth empty to left
        width = sx + sw - sw * 7  `div` 64 - xpos -- and c`div`d to right
                                                  -- (space for .xinіtrc bar and tray)

{-    -- experimental slave lines config --
      -- (scrolling sometimes fails with dzen stuck uncollapsed)
      --          
      -- instead of the above, use something like 
      --                                          
      ++ " -l 5 -e 'entertitle=uncollapse,grabkeys;\
         \ enterslave=grabkeys;leaveslave=collapse,ungrabkeys;\
         \ button1=menuexec;button2=togglestick;button3=exit:13;\
         \ button4=scrollup;button5=scrolldown;\
         \ key_Escape=ungrabkeys,exit;onstart=lower'"
      --                                          
      --   and write to it with something like
      --   hPutStrLn dz::System.IO.Handle dzSlaveLines::String -}

stripDzen :: String -> String -- strip dzen formatting to undo ppHidden, corrected parse
stripDzen = strip [] where
    strip keep [] = keep
    strip keep ('^':'^':x) = strip (keep ++ "^") x
    strip keep ('^':x) = strip keep (drop 1 . dropWhile (')' /=) $ x)
    strip keep x = let (good,x') = span ('^' /=) x
        in strip (keep ++ good) x'

xpm :: String -> String -> String
xpm path = wrap ("^i(" ++ path ++ "/.dzen/icons/") ".xpm)"


dzdarkPP  hm dzIn = defaultPP
    { ppCurrent         = wrap (dzfg hlightCol  "^p(;+7)^r(5x5)^p(+2;-7)") ""  . dzfg currentCol
    , ppVisible         = dzfg visibleCol . ("^p(;+7)^ro(5x5)^p(+2;-7)" ++)
    , ppHidden          = dzfg hiddenCol
    , ppHiddenNoWindows = dzfg fadedCol
    , ppUrgent          = dzfg urgentCol . stripDzen
    , ppWsSep           = kernedsp
    , ppSep             = ""
    , ppExtras          = [xmonicon]
    , ppOrder           = \(ws:_:_:xs) ->  ["^tw()    "] ++ [ws] ++ xs -- see [ppOrder]
    , ppOutput          = hPutStrLn dzIn
    }  
  where kernedsp = "^p(+12)"
        dzfg c   = dzenColor (c dkCols) "" -- (Colorset -> String) -> String -> String
        xmonicon = io $ return . Just $
            kernedsp ++ "^p(;4)" ++ xpm hm "xmonad16" ++ "^p(;-4)" ++ kernedsp


dzlightPP hm dzIn = defaultPP
    { ppCurrent         = wrap (dzfg hlightCol  "^p(;+2)^r(3x3)^p(+2;-2)") "" . dzfg currentCol
    , ppVisible         = dzfg visibleCol . ("^p(;+2)^ro(3x3)^p(+2;-2)" ++)
    , ppHidden          = dzfg hiddenCol
    , ppHiddenNoWindows = dzfg fadedCol
    , ppUrgent          = dzfg urgentCol . stripDzen
    , ppWsSep           = kernedsp
    , ppSep             = ""
    , ppExtras          = [xmonicon]
    , ppOrder           = \(ws:_:_:xs) ->  ["^tw()    "] ++ [ws] ++ xs -- see [ppOrder]
    , ppOutput          = hPutStrLn dzIn
    }
  where kernedsp = "^p(+7)"
        dzfg c   = dzenColor (c ltCols) ""
        xmonicon = io $ return . Just $
            kernedsp ++ "^p(;-1)" ++ xpm hm "xmonad16" ++ "^p(;+1)" ++ kernedsp

    -- [ppOrder] -- using ^tw() plus " -l 7 -e '...uncollapse...˙"
    -- in dzen params allows use of e.g. `hPutStrLn dzHandle dzSlaveLines' to
    -- write to slave window, but on some setups mouse scrolling messes up.
    -- I find titles and layouts jumping around in the status bar distracting,
    -- but if you want them use something like
    -- ppOrder = \(ws:l:t:xs) ->  [l,ws,t] ++ xs
