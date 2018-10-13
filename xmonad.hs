--
-- File     : ~/.xmonad/xmonad.hs (for Xmonad >= 0.9)
-- Author   : Thayer Williams
-- Website  : http://cinderwick.ca/
-- Desc     : A simple, mouse-friendly xmonad config geared towards
--            netbooks and other low-resolution devices.
--
--            dzen is used for statusbar rendering, with optional mouse
--            integration provided by xdotool:
--
--             * left-click workspace num to go to that ws
--             * left-click layout to cycle next layout
--             * left-click window title to cycle next window
--             * middle-click window title to kill focused window
--

import XMonad
import XMonad.Actions.CycleWindows -- classic alt-tab
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Actions.DwmPromote   -- swap master like dwm
import XMonad.Hooks.DynamicLog     -- statusbar 
import XMonad.Hooks.EwmhDesktops   -- fullscreenEventHook fixes chrome fullscreen
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.UrgencyHook    -- window alert bells 
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Layout.ResizableTile -- resizable windows
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run(spawnPipe)  -- spawnPipe and hPutStrLn
import System.IO                   -- hPutStrLn scope
import qualified XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.MultiToggle


import qualified XMonad.StackSet as W   -- manageHook rules
import qualified Data.Map as M

main = do
        status <- spawnPipe myDzenStatus    -- xmonad status on the left
        conky  <- spawnPipe myDzenConky     -- conky stats on the right
        xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig 
            { modMask            = mod4Mask
            , terminal           = "gnome-terminal"
            , borderWidth        = 2
            , normalBorderColor  = "#dddddd"
            , focusedBorderColor = "#0000ff"
            , handleEventHook    = fullscreenEventHook
            , workspaces = myWorkspaces
            , layoutHook = myLayoutHook
            , manageHook = manageDocks <+> myManageHook
                            <+> manageHook defaultConfig
            , logHook    = myLogHook status
            , keys = \c -> myKeys c `M.union` keys defaultConfig c
            } 
--            `additionalKeysP` myKeys

-- Tags/Workspaces
-- clickable workspaces via dzen/xdotool
myWorkspaces            :: [String]
myWorkspaces            = clickable . (map dzenEscape) $ ["u"] ++ (map show [1 .. 9]) ++ ["0", "m", "v"]
 
  where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]

-- Layouts
-- the default layout is fullscreen with smartborders applied to all
myLayoutHook = avoidStruts $ smartBorders ( tiled ||| full ) -- ||| mtiled )
  where
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled
    --tiled   = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    tiled   = named "T" $ ResizableTall 1 (4/100) (2/(1+(toRational(sqrt(5)::Double)))) []
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

-- Window management
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
--    , className =? "Vlc"            --> doFloat
    , className =? "Gimp"           --> doFloat
--    , className =? "XCalc"          --> doFloat
--    , className =? "Chromium"       --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
--    , className =? "Nautilus"       --> doF (W.shift (myWorkspaces !! 2)) -- send to ws 3
--    , className =? "Gimp"           --> doF (W.shift (myWorkspaces !! 3)) -- send to ws 4
    , className =? "stalonetray"    --> doIgnore
    ]

-- Statusbar 
--
myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myDzenStatus = "dzen2 -w '720' -ta 'l'" ++ myDzenStyle
-- myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '320' -w '704' -ta 'r'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '720' -w '880' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -h '20' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=11'"
--myDzenStyle  = " -h '20' -fg '#777777' -bg '#ff0f0f' -fn 'arial:bold:size=11'"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#3399ff" "" . wrap " " " "
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = "     "
    , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#ffffff" "" 
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 64 . dzenEscape
    }

-- Key bindings
--
myKeys conf@(XConfig {modMask = modm}) = M.fromList $ 
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) ([xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0,xK_minus, xK_equal])
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] 
  ++ 
  [ ((modm, xK_Right), nextWS)
  , ((modm, xK_Left ), prevWS)
  -- toggles: fullscreen, flip x, flip y
  , ((modm .|. controlMask, xK_space), sendMessage TL.ToggleLayout)
--  , ((modm .|. controlMask, xK_x),     sendMessage $ Toggle REFLECTX)
--  , ((modm .|. controlMask, xK_y),     sendMessage $ Toggle REFLECTY)
  
  , ((modm, xK_z), sendMessage MirrorShrink)
  , ((modm, xK_a), sendMessage MirrorExpand)
  ]
  ++
  [ ((modm, xK_b), sendMessage ToggleStruts)
  , ((modm, xK_p), spawn "gmrun"           ) -- app launcher
  , ((modm, xK_Return), dwmpromote         ) -- swap the focused window and the master window
  , ((modm, xK_Tab), toggleWS              ) -- toggle last workspace (super-tab)
  , ((modm .|. shiftMask, xK_n), spawn "chromium-browser")
  , ((modm .|. controlMask, xK_l), spawn "gnome-screensaver-command --lock") -- lock screen
  , ((modm .|. controlMask, xK_s), spawn "/home/czechow/bin/vm-ctl pause; sleep 1; sudo /usr/sbin/pm-suspend") -- go to sleep
  ]
--         [ ("M-b"        , sendMessage ToggleStruts              ) -- toggle the status bar gap
--         , ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
--         , ("M-S-<Return>", spawn "gnome-terminal")
--         , ("M-<Return>" , dwmpromote                            ) -- swap the focused window and the master window
--         , ("M-<Tab>"    , toggleWS                              ) -- toggle last workspace (super-tab)
--         , ("M-<Right>"  , nextWS                                ) -- go to next workspace
--         , ("M-<Left>"   , prevWS                                ) -- go to prev workspace
--         , ("M-S-<Right>", shiftToNext                           ) -- move client to next workspace
--         , ("M-S-<Left>" , shiftToPrev                           ) -- move client to prev workspace
----         , ("M-c"        , spawn "xcalc"                         ) -- calc
--         , ("M-p"        , spawn "gmrun"                         ) -- app launcher
-- --        , ("M-n"        , spawn "wicd-client -n"                ) -- network manager
--         , ("M-r"        , spawn "xmonad --restart"              ) -- restart xmonad w/o recompiling
--         , ("M-S-n"      , spawn "chromium-browser"              ) -- launch browser
--         , ("M-S-w"      , spawn "chromium-browser --incognito"  ) -- launch private browser
----         , ("M-n"        , spawn "nautilus"                      ) -- launch file manager
----         , ("M-s"        , spawn "urxvtcd -e bash -c 'screen -dRR -S $HOSTNAME'" ) -- launch screen session
----         , ("C-M1-<Delete>" , spawn "sudo shutdown -r now"       ) -- reboot
----         , ("C-M1-<Insert>" , spawn "sudo shutdown -h now"       ) -- poweroff
--         ]

-- vim:sw=4 sts=4 ts=4 tw=0 et ai 
