import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))
import XMonad.Actions.CopyWindow
import XMonad.Actions.FocusNth
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.Minimize
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Run

import System.IO
import Graphics.X11.Types

-- Keysyms missing in XMonad
xK_XF86AudioLowerVolume = 0x1008ff11
xK_XF86AudioRaiseVolume = 0x1008ff13
xK_XF86AudioMute = 0x1008FF12
xK_XF86MonBrightnessUp = 0x1008FF02
xK_XF86MonBrightnessDown = 0x1008FF03
xK_XF86Launch1 = 0x1008ff41
xK_XFAudioPlay = 0x1008FF14
xK_XFAudioStop = 0x1008FF15
xK_XFAudioNext = 0x1008FF17
xK_XFAudioPrev = 0x1008FF16
xK_XF86ScreenSaver = 0x1008FF2D

-- Prompt configuration
myXPConfig = defaultXPConfig

-- My extra prompts
screenshotPrompt :: XPConfig -> X ()
screenshotPrompt c =
  inputPrompt c "Screenshot name" ?+ \name ->
  io $ spawn ("screenshot " ++ name)

myWorkspaceKeys = [xK_u, xK_i, xK_a, xK_e, xK_o]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.

myKeys sp conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask, xK_space ), spawnHere sp $ XMonad.terminal conf)

    -- launch start prompt
    , ((modMask, xK_p), shellPromptHere sp myXPConfig)

    -- launch GUI browser
    , ((modMask, xK_w ), spawnHere sp "exec x-www-browser")

    -- launch screen locker
    , ((0, xK_XF86ScreenSaver ), spawnHere sp "exec catlock")

    -- launch drama
    , ((modMask, xK_XF86Launch1 ), spawn "exec python /home/timo/.drama/drama.py")

    -- launch sm
    , ((modMask .|. shiftMask, xK_XF86Launch1 ), spawnHere sp "sm")

    -- launch clipreduce
    , ((modMask .|. shiftMask, xK_c), spawn "exec clipreduce.sh" )
    -- and clipedit
    , ((modMask, xK_c), spawnHere sp "exec urxvt -e editclip.sh")
    -- and clipspacify
    , ((modMask .|. controlMask, xK_c), spawn "exec clip_spacify.py")

    -- monitor backlight
    -- , ((0, xK_XF86MonBrightnessUp), spawn "xbacklight -steps 1 -time 1 -inc 10")
    -- , ((0, xK_XF86MonBrightnessDown), spawn "xbacklight -steps 1 -time 1 -dec 10")

  --  -- navigate MPD tracks
  --  , ((0, xK_XFAudioNext), spawn "exec mpc next")
  --  , ((0, xK_XFAudioPrev), spawn "exec mpc prev")
  --  , ((0, xK_XFAudioPlay), spawn "exec mpc toggle")
  --  , ((0, xK_XFAudioStop), spawn "exec mpc stop")
    -- navigate xmms2 tracks
    , ((0, xK_XFAudioNext), spawn "exec xmms2 next")
    , ((0, xK_XFAudioPrev), spawn "exec xmms2 prev")
    , ((0, xK_XFAudioPlay), spawn "exec xmms2 toggle")
    , ((0, xK_XFAudioStop), spawn "exec xmms2 stop")

    -- make screenshot
--    , ((0, xK_Print), screenshotPrompt myXPConfig)

    -- launch xkill
    , ((modMask, xK_k), spawn "exec xkill")

    -- raise audio volume
 --   , ((0, xK_XF86AudioRaiseVolume), io $ hPutStrLn amixer "set Master 5%+")
    , ((0, xK_XF86AudioRaiseVolume), spawn "ossmix misc.pcm1 +5")
 --   , ((shiftMask, xK_XF86AudioRaiseVolume), io $ hPutStrLn amixer "set PCM 5%+")
 --   , ((shiftMask, xK_XF86AudioRaiseVolume), io $ hPutStrLn amixer "set PCM 5%+")
 --   , ((controlMask, xK_XF86AudioRaiseVolume), spawn "mpc volume +5")
    , ((controlMask, xK_XF86AudioRaiseVolume), spawn "xmms2 server volume +5")

    -- mute key
  --  , ((0, xK_XF86AudioMute), io $ hPutStrLn amixer "set Master toggle")

    -- lower audio volume
 --   , ((0, xK_XF86AudioLowerVolume), io $ hPutStrLn amixer "set Master 5%-")
    , ((0, xK_XF86AudioLowerVolume), spawn "ossmix misc.pcm1 -- -5")
 --   , ((shiftMask, xK_XF86AudioLowerVolume), io $ hPutStrLn amixer "set PCM 5%-")
 --   , ((controlMask, xK_XF86AudioLowerVolume), spawn "mpc volume -5")
    , ((controlMask, xK_XF86AudioLowerVolume), spawn "xmms2 server volume -5")

    -- close focused window
    , ((modMask, xK_q), kill)

    -- toggle fullscreen
    , ((modMask, xK_f), sendMessage (XMonad.Layout.ToggleLayouts.Toggle "Full"))

     -- Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_Return), sendMessage NextLayout)

    -- Move focus to the next window
    , ((modMask, xK_n), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask, xK_r), windows W.focusUp)

    -- minimize and unminimize windows
    , ((modMask,               xK_m), withFocused (\f -> sendMessage (MinimizeWin f)))
    , ((modMask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)

    -- Swap the focused window and the master window
    , ((modMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_n), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_r), windows W.swapUp)

    -- Shrink the master area
    , ((modMask .|. shiftMask, xK_s), sendMessage Shrink)

    -- Expand the master area
    , ((modMask .|. shiftMask, xK_t), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask .|. shiftMask, xK_space), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
 --   , ((modMask .|. shiftMask, xK_s ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
 --   , ((modMask .|. shiftMask, xK_t), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    , ((modMask, xK_b), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modMask .|. shiftMask .|. controlMask, xK_x), io (exitWith ExitSuccess))

    -- Make window sticky
     , ((modMask, xK_l ), windows copyToAll)

    -- Unstick window
     , ((modMask .|. shiftMask, xK_l),  killAllOtherCopies)

    -- Restart xmonad
    , ((modMask , xK_x), restart "xmonad" True)
    ]
    ++

    -- mod-F[k..c], Switch to workspace N
    -- mod-shift-F[k..c], Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    ++
    -- mod-ctrl-F[k..c] Exchange workspaces
    [((modMask .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaceKeys]

    ++
    -- mod4-[1..9] Switch to window N
    [((modMask, k), focusNth i)
        | (i, k) <- zip [0 .. 8] [xK_1 ..]]

    ++
    -- mod-{s,t}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{s,t}, Move client to screen 1 or 2
    [((modMask .|. m, key), f sc)
        | (key, sc) <- zip [xK_s, xK_t] [0..]
        , (f, m) <- [(viewScreen, 0)]]

------------------------------------------------------------------------
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
mySplit = magnifiercz' 1.4 $ Tall nmaster delta ratio
    where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 60/100

myLayout = smartBorders (avoidStruts (toggleLayouts Full (withIM (1%7) (ClassName "Pidgin") (minimize ((GridRatio 1.2) ||| tiled ||| tabbed shrinkText defaultTheme ||| mySplit)))))
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio = 1/2

     -- Percent of screen to increment by when resizing panes
     delta = 3/100

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
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "feh" --> doFloat
    , className =? "Wine" --> doFloat
    , className =? "Vncviewer" --> doFloat
    , className =? "VLC" --> doFloat
    , className =? "Xmessage" --> doFloat
    , title =? "Irssi" --> doF W.shiftMaster
    , doF avoidMaster
    , manageDocks
    ]

-- Don't replace master window with new window
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    otherwise           -> c


myPP h = dzenPP { ppOutput = hPutStrLn h
                  , ppCurrent = dzenColor "green" "" . pad
                  , ppVisible = dzenColor "blue" "" . pad
                  , ppHidden = dzenColor "" "" . pad
                  , ppHiddenNoWindows = const ""
                  , ppUrgent = dzenColor "red" ""
                  , ppWsSep = ""
                  , ppSep = ""
                  , ppLayout = dzenColor "grey" "" . pad
                  , ppTitle = dzenEscape
                 }

main = do
    sp <- mkSpawner
    din <- spawnPipe "exec dzen2 -bg black -fg grey -fn fixed -ta l -w 800"
    xmonad $
      withUrgencyHookC dzenUrgencyHook {
        args = ["-fn", "fixed", "-bg", "black", "-fg", "lightgrey", "-xs", "1"],
        duration = seconds 1
      } urgencyConfig {
        remindWhen = Every 120 
      }
      $ ewmh $ defaultConfig {
      -- simple stuff
        terminal = "urxvt",
        focusFollowsMouse = True,
        borderWidth = 1,
        modMask = mod4Mask,
        numlockMask = mod2Mask,
        workspaces = ["u","i","a","e","o"],
        normalBorderColor = "darkblue",
        focusedBorderColor = "red",

      -- key bindings
        keys = myKeys sp,
        mouseBindings = myMouseBindings,

      -- hooks, layouts
        layoutHook = myLayout,
        manageHook = manageSpawn sp <+> myManageHook,
        logHook = dynamicLogWithPP (myPP din)
                  >> updatePointer (Relative 0.5 0.5)
    }

