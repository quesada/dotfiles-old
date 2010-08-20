-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Brent_Yorgey%27s_darcs_xmonad.hs
import XMonad                          -- (0) core xmonad libraries

import qualified XMonad.StackSet as W  -- (0a) window stack manipulation
import qualified Data.Map as M         -- (0b) map creation
import Data.List (isPrefixOf, (\\))
import Data.Maybe (isNothing, isJust, catMaybes)

import System.Posix.Unistd

-- Hooks -----------------------------------------------------

import XMonad.Hooks.DynamicLog     -- (1)  for dzen status bar
  hiding (pprWindowSet)
import XMonad.Hooks.UrgencyHook    -- (2)  alert me when people use my nick
                                   --      on IRC
import XMonad.Hooks.ManageDocks    -- (3)  automatically avoid covering my
                                   --      status bar with windows
import XMonad.Hooks.ManageHelpers  -- (4)  for doCenterFloat, put floating
                                   --      windows in the middle of the
                                   --      screen

-- Layout ----------------------------------------------------

import XMonad.Layout.ResizableTile -- (5)  resize non-master windows too
import XMonad.Layout.Grid          -- (6)  grid layout
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders     -- (7)  get rid of borders sometimes
                                   -- (8)  navigate between windows
import XMonad.Layout.WindowNavigation  --  directionally
import XMonad.Layout.Named         -- (9)  rename some layouts
import XMonad.Layout.PerWorkspace  -- (10) use different layouts on different WSs
import XMonad.Layout.WorkspaceDir  -- (11) set working directory
                                   --      per-workspace
import XMonad.Layout.Reflect       -- (13) ability to reflect layouts
import XMonad.Layout.MultiToggle   -- (14) apply layout modifiers dynamically
import XMonad.Layout.MultiToggle.Instances
                                   -- (15) ability to magnify the focused
                                   --      window
import qualified XMonad.Layout.Magnifier as Mag

import XMonad.Layout.Gaps

-- Actions ---------------------------------------------------

import XMonad.Actions.CycleWS      -- (16) general workspace-switching
                                   --      goodness
import XMonad.Actions.CycleRecentWS
                                   -- (17) more flexible window resizing
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.Warp         -- (18) warp the mouse pointer
import XMonad.Actions.Submap       -- (19) create keybinding submaps
import XMonad.Actions.Search       -- (20) some predefined web searches
import XMonad.Actions.WindowGo     -- (21) runOrRaise
import XMonad.Actions.UpdatePointer -- (22) auto-warp the pointer to the LR
                                    --      corner of the focused window
import XMonad.Actions.WithAll
import XMonad.Actions.SpawnOn

import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces

-- Prompts ---------------------------------------------------

import XMonad.Prompt                -- (23) general prompt stuff.
import XMonad.Prompt.Man            -- (24) man page prompt
import XMonad.Prompt.AppendFile     -- (25) append stuff to my NOTES file
import XMonad.Prompt.Ssh            -- (26) ssh prompt
import XMonad.Prompt.Input          -- (26) generic input prompt, used for
                                    --      making more generic search
                                    --      prompts than those in
                                    --      XMonad.Prompt.Search
import XMonad.Prompt.Workspace

-- Utilities -------------------------------------------------

import XMonad.Util.Loggers          -- (28) some extra loggers for my
                                    --      status bar
import XMonad.Util.EZConfig         -- (29) "M-C-x" style keybindings
import XMonad.Util.NamedScratchpad  -- (30) 'scratchpad' terminal
import XMonad.Util.Run              -- (31) for 'spawnPipe', 'hPutStrLn'

import Control.Monad (when)

                                                                -- (31)
main = do h <- spawnPipe "dzen2 -ta r -fg '#a8a3f7' -bg '#3f3c6d' -e 'onstart=lower'"
          host <- getHost
          checkTopicConfig myTopicNames myTopicConfig
          xmonad $ byorgeyConfig h host                         -- (0)

data Host = Desktop | Laptop Bool -- ^ Does the laptop have a Windows key?
  deriving (Eq, Read, Show)

getHost :: IO Host
getHost = do
  hostName <- nodeName `fmap` getSystemID
  return $ case hostName of
    "archimedes" -> Laptop True
    "euclid"     -> Laptop False
    "LVN513-12"  -> Desktop
    _            -> Desktop

myTerminal = "urxvt --perl-lib ~/.urxvt -fg lightgrey -bg black +sb"
myShell = "zsh"

byorgeyConfig h host = myUrgencyHook $                         -- (2)
     defaultConfig
       {
         borderWidth        = 2
       , terminal           = myTerminal
       , workspaces         = myTopicNames
       , modMask            = if host == Laptop False
                                then modMask defaultConfig
                                else mod4Mask

       , normalBorderColor  = "#dddddd"
       , focusedBorderColor = "#0033ff"
                                                                -- (22)
       , logHook            = myDynamicLog h host
       , manageHook         = manageSpawn
                              <+> myManageHook
                              <+> manageHook defaultConfig
       , layoutHook         = myLayoutHook
       , focusFollowsMouse  = False

         -- XXX fixme: comment!                                 -- (29)
       , startupHook        = return () >>
                              checkKeymap (byorgeyConfig h host)
                                          (myKeys h host)

       }
       `additionalKeysP` (myKeys h host)                        -- (29)

checkWS :: (String -> Bool) -> String -> X ()
checkWS p w = do cw <- gets (W.currentTag . windowset)
                 when (not $ p cw) $ (windows $ W.greedyView w)

-- have urgent events flash a yellow dzen bar with black text
myUrgencyHook = withUrgencyHook dzenUrgencyHook                 -- (2)
    { args = ["-bg", "yellow", "-fg", "black"] }

data TopicItem = TI { topicName :: Topic
                    , topicDir  :: Dir
                    , topicAction :: X ()
                    }

-- define some custom topics for use with the TopicSpace module.
myTopics :: [TopicItem]
myTopics = [ TI "web" "" (spawn "firefox")
           , TI "irc" "" (safeRunInTerm "" "ssh lvn")
           , TI "mail" "" (safeRunInTerm "" "ssh enx")
           , TI "read" "papers" spawnShell
           , TI "write" "" spawnShell
           , TI "org" "notes"
              (spawn "emacs --name org ~/notes/`date +%Y-%m-%d`.org")
           , TI "draw" "" (spawn "inkscape")
           , TI "xm-conf" ".xmonad"
              (edit "~/.xmonad/xmonad.hs" >>
               spawnShell)
           , TI "xm-hack" "xmonad/XMonadContrib" spawnShell
           , TI "em-conf" "" (edit "~/.emacs")
           , TI "music" "music" (spawn "rhythmbox")
           , TI "net" "" (spawn "wicd-client -n" >>
                          spawnShell)
           , TI "conf" "" spawnShell
           , TI "misc" "" spawnShell
           , TI "120" "teaching/120/09fa" spawnShell
           , TI "ref" "reference" spawnShell
           , TI "play" "" spawnShell
           , TI "tex-conf" "texmf/tex" (edit "~/texmf/tex/brent.sty")
           , TI "mlt" "teaching/mlt" spawnShell
           , TI "MR" "writing/Monad.Reader" spawnShell
           ]

edit :: String -> X ()
edit = spawn . ("em "++)

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs = M.fromList $ map (\(TI n d _) -> (n,d)) myTopics
  , defaultTopicAction = const (return ())
  , defaultTopic = "web"
  , maxTopicHistory = 10
  , topicActions = M.fromList $ map (\(TI n _ a) -> (n,a)) myTopics
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ "'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedGotoOtherScreen :: X ()
promptedGotoOtherScreen =
  workspacePrompt myXPConfig $ \ws -> do
    nextScreen
    goto ws

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

-- XXX offset scratchpad windows by a bit --- each one different?
scratchpadSize = W.RationalRect (1/4) (1/4) (1/2) (1/2)
mySPFloat = customFloating scratchpadSize

scratchpads =
  [ NS "term" "urxvt-custom -title term" (title =? "term") mySPFloat
  , NS "ghci" "urxvt-custom -e ghci" (title =? "ghci") mySPFloat
  , NS "sync" "urxvt-custom -e sy" (title =? "sy") mySPFloat
  , NS "top"  "urxvt-custom -e htop" (title =? "htop") mySPFloat
  ]

myDynamicLog h host = dynamicLogWithPP $ byorgeyPP              -- (1)
  { ppVisible = dzenColor "blue" "#a8a3f7" . pad
  , ppExtras = [ date "%a %b %d  %I:%M %p"                      -- (1,28)
               , loadAvg                                        -- (28)
               ]
               ++ (case host of Laptop _ -> [battery]
                                _      -> [])
  , ppOrder  = \(ws:l:t:exs) -> [t,l,ws]++exs                   -- (1)
  , ppOutput = hPutStrLn h                                      -- (1,31)
  , ppTitle  = shorten (case host of Laptop _ -> 45
                                     Desktop  -> 60)
  , ppSort   = fmap (namedScratchpadFilterOutWorkspace.) (ppSort byorgeyPP)
  , ppHiddenNoWindows = const ""
  }

-- my custom keybindings.
myKeys h host = myKeymap host (byorgeyConfig h host)

myKeymap host conf =

    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    -- mod-ctrl-[1..],  Switch to workspace N on other screen
    [ (m ++ "M-" ++ [k], f i)                                   -- (0)
        | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=[]\\" -- (0)
        , (f, m) <- [ (windows . W.view, "")                    -- (0a)
                    , (windows . W.shift, "S-")
                    , (\ws -> nextScreen >> (windows . W.view $ ws), "C-")
                    ]
    ]

    ++
    [ ("M-S-x", spawnShell)                          -- (0)
    , ("M-S-b", spawn "urxvt-big")
    , ("M-g",   promptedGoto)
    , ("M-C-g", promptedGotoOtherScreen)
    , ("M-S-g", promptedShift)
    , ("M-S-C-g", workspacePrompt myXPConfig $ withAll' . W.shiftWin)

      -- in conjunction with manageHook, open a small temporary
      -- floating terminal
    , ("M-a s", namedScratchpadAction scratchpads "term")       -- (30)
    , ("M-a g", namedScratchpadAction scratchpads "ghci")
    , ("M-a t", namedScratchpadAction scratchpads "top")
    ]
    ++ (case host of Laptop _ ->
                       [("M-a y", namedScratchpadAction scratchpads "sync")]
                     _ -> []
       )
    ++
    [ ("M-S-a", kill)                                           -- (0)
    , ("M-S-C-a", killAll)

    -- some gap-toggling
    , ("M-C-p b", sendMessage $ ToggleStrut D)                    -- (3)
    , ("M-C-p t", sendMessage $ ToggleStrut U)                    --  "
    , ("M-C-p a", sendMessage $ ToggleStruts)                     --  "

    , ("M-C-p g", sendMessage $ ToggleGaps)
    ]

    ++
    [ ("M-C-p " ++ f ++ " <" ++ dk ++ ">", sendMessage $ m d)
        | (dk, d) <- [("L",L), ("D",D), ("U",U), ("R",R)]
        , (f, m)  <- [("v", ToggleGap), ("h", IncGap 10), ("f", DecGap 10)]
    ]

    ++
    -- rotate workspaces.
    [ ("M-C-<R>",   nextWS )                                    -- (16)
    , ("M-C-<L>",   prevWS )                                    --  "
    , ("M-S-<R>",   shiftToNext )                               --  "
    , ("M-S-<L>",   shiftToPrev )                               --  "
    , ("M-S-C-<R>", shiftToNext >> nextWS )                     --  "
    , ("M-S-C-<L>", shiftToPrev >> prevWS )                     --  "
    , ("M-<R>",     moveTo Next HiddenNonEmptyWS)               --  "
    , ("M-<L>",     moveTo Prev HiddenNonEmptyWS)               --  "
    , ("M-f",       newCodeWS)                                  --  "

    -- expand/shrink windows
    , ("M-r k", sendMessage MirrorExpand)                       -- (5)
    , ("M-r j", sendMessage MirrorShrink)                       -- (5)
    , ("M-r h", sendMessage Shrink)                             -- (0)
    , ("M-r l", sendMessage Expand)                             -- (0)

    -- switch to previous workspace
    , ("M-z", toggleWS)                                         -- (16)
    , ("M-S-z", killAll >> moveTo Prev HiddenNonEmptyWS)

    -- dynamic workspace bindings
    , ("M-n", addWorkspacePrompt myXPConfig)
    , ("M-C-r", removeWorkspace)
    , ("M-C-S-r", killAll >> removeWorkspace)

    -- move between screens
    , ("M-s", nextScreen)
    , ("M-w", swapNextScreen)
    , ("M-e", shiftNextScreen)

      -- lock the screen with xscreensaver
    , ("M-S-l", spawn "xscreensaver-command -lock")             -- (0)

    -- bainsh the pointer
    , ("M-'", banishScreen LowerRight)                          -- (18)
    , ("M-b", warpToWindow (1/2) (1/2))

    -- some programs to start with keybindings.
    , ("M-x f", spawnOn "web" "firefox")
    , ("M-x o", spawnOn "web" "opera")
    , ("M-x g", spawnOn "draw" "gimp")                          -- (0)
    , ("M-x m", spawn "rhythmbox")                              -- (0)
    , ("M-x t", spawn "xclock -update 1")                       -- (0)
    , ("M-x S-g", spawn "javaws ~/playing/go/cgoban.jnlp")      -- (0)
    , ("M-x n", goto "org")

    -- configuration.
    , ("M-c x", goto "xm-conf")
    , ("M-c e", goto "em-conf")
    , ("M-c t", goto "tex-conf")
    ] ++
    (case host of Laptop _ -> [("M-c n", goto "net")]
                  _        -> [])
    ++
    [ ("M-c v", spawn "urxvt -e alsamixer")                     -- (0)
    , ("M-c k", spawn "xkill")
    , ("M-c M-S-a", killAll)

    -- window navigation keybindings.
    , ("C-<R>", sendMessage $ Go R)                             -- (8)
    , ("C-<L>", sendMessage $ Go L)                             --  "
    , ("C-<U>", sendMessage $ Go U)                             --  "
    , ("C-<D>", sendMessage $ Go D)                             --  "
    , ("S-C-<R>", sendMessage $ Swap R)                         --  "
    , ("S-C-<L>", sendMessage $ Swap L)                         --  "
    , ("S-C-<U>", sendMessage $ Swap U)                         --  "
    , ("S-C-<D>", sendMessage $ Swap D)                         --  "

    -- switch to urgent window
    , ("M-u", focusUrgent)

    -- toggles: fullscreen, flip x, flip y, mirror, no borders
    , ("M-C-<Space>", sendMessage $ Toggle NBFULL)              -- (14)
    , ("M-C-x",       sendMessage $ Toggle REFLECTX)            -- (14,13)
    , ("M-C-y",       sendMessage $ Toggle REFLECTY)            -- (14,13)
    , ("M-C-m",       sendMessage $ Toggle MIRROR)              --  "
    , ("M-C-b",       sendMessage $ Toggle NOBORDERS)           --  "

    -- some prompts.
      -- ability to change the working dir for a workspace.
    , ("M-p d", changeDir myXPConfig)                           -- (11)
      -- man page prompt
    , ("M-p m", manPrompt myXPConfig)                           -- (24)
      -- add single lines to my NOTES file from a prompt.       -- (25)
    , ("M-p n", appendFilePrompt myXPConfig "$HOME/NOTES")
      -- shell prompt.
    , ("M-p s", sshPrompt myXPConfig)                         -- (26)

      -- some searches.
    , ("M-/", submap . mySearchMap $ myPromptSearch)            -- (19,20)
    , ("M-C-/", submap . mySearchMap $ mySelectSearch)          -- (19,20)

    -- some random utilities.
    , ("M-C-c", spawn "dzen-cal")  -- calendar
    , ("<Print>", spawn "scrot")
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")
    ]

newCodeWS :: X ()
newCodeWS = withWindowSet $ \w -> do
  let wss = W.workspaces w
      cws = map W.tag $ filter (\ws -> "code" `isPrefixOf` W.tag ws && isJust (W.stack ws)) wss
      num = head $ [0..] \\ catMaybes (map (readMaybe . drop 4) cws)
      new = "code" ++ show num
  when (new `notElem` (map W.tag wss)) $ addWorkspace new
  windows $ W.view new
 where readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing

-- Perform a search, using the given method, based on a keypress
mySearchMap method = M.fromList $                               -- (0b)
        [ ((0, xK_g), method google)                            -- (20)
        , ((0, xK_w), method wikipedia)                         --  "
        , ((0, xK_h), method hoogle)                            --  "
        , ((shiftMask, xK_h), method hackage)
        , ((0, xK_s), method scholar)                           --  "
        , ((0, xK_m), method mathworld)                         --  "
        , ((0, xK_p), method maps)                              --  "
        , ((0, xK_d), method dictionary)                        --  "
        , ((0, xK_a), method alpha)                             --  "
        , ((0, xK_l), method lucky)                             --  "
        , ((0, xK_i), method images)
        ]

-- Prompt search: get input from the user via a prompt, then
--   run the search in firefox and automatically switch to the "wb"
--   workspace
myPromptSearch (SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ \s ->                    -- (27)
      (search "firefox" site s >> viewWeb)                      -- (0,20)

-- Select search: do a search based on the X selection
mySelectSearch eng = selectSearch eng >> viewWeb                -- (20)

-- Switch to the "web" workspace
viewWeb = windows (W.view "web")                                -- (0,0a)

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig                                    -- (23)
    { fgColor = "#a8a3f7"
    , bgColor = "#3f3c6d"
    }

-- Set up a customized manageHook (rules for handling windows on
--   creation)
myManageHook :: ManageHook                                      -- (0)
myManageHook = composeAll $
                   -- auto-float certain windows
                 [ className =? c --> doCenterFloat | c <- myFloats ] -- (4)
                 ++
                 [ fmap (t `isPrefixOf`) title --> doFloat | t <- myFloatTitles ]
                 ++
                   -- send certain windows to certain workspaces
                 [ className =? "Rhythmbox" --> doF (W.shift "music") -- (0,0a)
                   -- unmanage docks such as gnome-panel and dzen
                 , manageDocks                                     -- (3)
                   -- manage the scratchpad terminal window
                 , namedScratchpadManageHook scratchpads           -- (30)
                 , appName =? "xbuffy-main" --> doFloatAt 0.92 0.66
                 , appName =? "xbuffy-aux"  --> doFloatAt 0.92 0.81
                 ]
    -- windows to auto-float
    where myFloats = [ "Volume"
                     , "XClock"
                     , "Network-admin"
                     , "Xmessage"
                     , "gnome-search-tool"
                     , "Qjackctl.bin"
                     , "Icfp"
                     , "Floating"
                     , "Game"
                     ]
          myFloatTitles = ["Bridge Bid", "Pong", "Floating"]

-- specify a custom layout hook.
myLayoutHook =

    -- automatically avoid overlapping my dzen status bar.
    avoidStrutsOn [U] $                                        -- (3)

    -- make manual gap adjustment possible.
    gaps (zip [U,D,L,R] (repeat 0)) $

    -- start all workspaces in my home directory, with the ability
    -- to switch to a new working dir.                          -- (10,11)
    workspaceDir "~" $

    -- navigate directionally rather than with mod-j/k
    configurableNavigation (navigateColor "#00aa00") $          -- (8)

    -- ability to toggle between fullscreen, reflect x/y, no borders,
    -- and mirrored.
    mkToggle1 NBFULL $                                  -- (14)
    mkToggle1 REFLECTX $                                -- (14,13)
    mkToggle1 REFLECTY $                                -- (14,13)
    mkToggle1 NOBORDERS $                               --  "
    mkToggle1 MIRROR $                                  --  "

    -- borders automatically disappear for fullscreen windows.
    smartBorders $                                              -- (7)

    -- "web" and "irc" start in Full mode and can switch to tiled...
    onWorkspaces ["web","irc"] (Full ||| myTiled) $               -- (10,0)

    -- ...whereas all other workspaces start tall and can switch
    -- to a grid layout with the focused window magnified.
    myTiled |||           -- resizable tall layout
    Mag.magnifier Grid |||                                      -- (15,6)
    TwoPane (3/100) (1/2)

-- use ResizableTall instead of Tall, but still call it "Tall".
myTiled = named "Tall" $ ResizableTall 1 0.03 0.5 []            -- (9,5)
