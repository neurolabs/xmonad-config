-- xmonad config used by Vic Fryzel
-- Author: Vic Fryzel
-- http://github.com/vicfryzel/xmonad-config
 
import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import Control.Monad (liftM2)


import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- Using konsole with Scratchpad gives a nice quake style multitab console.
myTerminal      = "konsole"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 1
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- second modmask for custom bindings
myOtherModMask  = mod1Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1:code","2:web","3:pmsg","4:mail","5:vm","6:misc"]
 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch gmrun
    , ((modMask .|. controlMask, xK_l     ), spawn "xscreensaver-command -lock")

    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "dmenu_run")
 
    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
 
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modMask,               xK_l     ), windows W.focusDown)
    , ((modMask,               xK_Right ), windows W.focusDown)
--    , ((shiftMask,             xK_Right ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusUp  )
    , ((modMask,               xK_Left  ), windows W.focusUp  )
--    , ((shiftMask,             xK_Left  ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_l     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modMask,               xK_y     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modMask,               xK_x     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- toggle the status bar gap
    -- TODO, update this binding with avoidStruts , ((modMask              , xK_b     ),
 
    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 
    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)

    -- All Apps in Grid View
    , ((modMask              , xK_g     ), goToSelected defaultGSConfig)

    -- alt space brings up yakuake
    --, ((mod1Mask             , xK_space ), runOrRaise "yakuake"  (title =? "scratch"))
    
    -- switch to prev/next workspace
    , ((modMask              , xK_i     ), moveTo Prev (WSIs notSP))
    , ((modMask              , xK_k     ), moveTo Next (WSIs notSP))
    , ((modMask .|. shiftMask, xK_i     ), shiftAndView Prev)
    , ((modMask .|. shiftMask, xK_k     ), shiftAndView Next)
    , ((modMask              , xK_Up    ), windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1)
    , ((modMask              , xK_Down  ), windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1)

    -- quake style console
    , ((myOtherModMask       , xK_space  ), scratchpadSpawnAction conf)

--    , ((0                    , 0x1008FF11), spawn "amixer -- sset Master playback 8%-")
--    , ((0                    , 0x1008FF13), spawn "amixer -- sset Master playback 8%+")
    , ((0                    , 0x1008FF11), spawn "volume -")
    , ((0                    , 0x1008FF13), spawn "volume +")
    
    ]
    ++
 
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  where 
    notSP = (return $ (\x -> x /= "NSP" && x /= "SP") . W.tag) :: X (WindowSpace -> Bool)
    -- | any workspace but scratchpad
    shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
            >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
    -- | hidden, non-empty workspaces less scratchpad
    shiftAndView' dir = findWorkspace getSortByIndexNoSP dir HiddenNonEmptyWS 1
            >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
    getSortByIndexNoSP =
            fmap (.scratchpadFilterOutWorkspace) getSortByIndex
    -- | toggle any workspace but scratchpad
    myToggle = windows $ W.view =<< W.tag . head . filter 
            ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden
    
 
-----------------------------------------------------------------------e
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myTabConfig = defaultTheme {   activeBorderColor = "#7C7C7C"
                             , activeTextColor = "#CEFFAC"
                             , activeColor = "#000000"
                             , inactiveBorderColor = "#7C7C7C"
                             , inactiveTextColor = "#EEEEEE"
                             , inactiveColor = "#000000"
                             , fontName = "xft:Terminus:pixelsize=9" }

myLayout = avoidStruts (tabbed shrinkText myTabConfig ||| tiled )
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 
     -- The default number of windows in the master pane
     nmaster = 1
 
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
 
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat | c <- myClassFloats ]
    , [ title =? t --> doFloat | t <- myTitleFloats ]
    , [ resource =? r --> doFloat | r <- myResourceFloats ]
    , [ resource =? r --> doIgnore | r <- myIgnores ]
    , [ className =? c --> viewShift "1:code" | c <- codeApps ]
    , [ className =? c --> viewShift "2:web" | c <- webApps ]
    , [ className =? c --> viewShift "3:pmsg" | c <- pmsgApps ]
    , [ className =? c --> viewShift "4:mail" | c <- mailApps ]
    , [ className =? c --> viewShift "5:vm" | c <- vmApps ]
    , [ scratchpadManageHook (W.RationalRect 0 0.016 1 0.985) ]
    ]
 where
   myClassFloats = ["MPlayer", "Gimp", "Smplayer", "Xdialog", "Kcalc", "Download"]
   myTitleFloats = ["alsamixer",".", "Firefox Preferences", "Selenium IDE", "Download"]
   myResourceFloats = ["compose"]
   myIgnores = ["desktop_window", "kdesktop", "stalonetray"]
   codeApps = ["Eclipse"]
   webApps = ["Firefox", "GNU IceCat", "Chrome", "Arora", "Firefox-bin", "Opera"]
   pmsgApps = ["XChat"]
   mailApps = ["Thunderbird"]
   vmApps = ["VirtualBox"]
   viewShift = doF . liftM2 (.) W.greedyView W.shift

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
 
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
	xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobar"
	xmonad $ defaults {
		logHook            = dynamicLogWithPP $ xmobarPP {
                                ppOutput = hPutStrLn xmproc
                                , ppTitle = xmobarColor "#FFB6B0" "" . shorten 100
                                , ppCurrent = xmobarColor "#CEFFAC" ""
                                , ppSep = "   "
                                , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByTag
                                }
		, manageHook = manageDocks <+> myManageHook
		, startupHook = setWMName "LG3D"
	}
 
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        numlockMask        = myNumlockMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = smartBorders $ myLayout,
        manageHook         = myManageHook,
        startupHook        = myStartupHook
    }
