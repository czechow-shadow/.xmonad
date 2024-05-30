{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import           XMonad
import qualified XMonad.StackSet              as SS

import qualified Data.Map.Strict              as M
import qualified Data.Set                     as S
import           Control.Monad

import qualified XMonad.Actions.CycleWS       as CWS
import           XMonad.Actions.DwmPromote    (dwmpromote)
import qualified XMonad.Config.Desktop        as CD
import qualified XMonad.Hooks.DynamicLog      as HDL
import qualified XMonad.Hooks.ManageDocks     as HMD
-- import           XMonad.Hooks.SetWMName       (setWMName)
import qualified XMonad.Layout.Fullscreen     as LF
import qualified XMonad.Layout.Named          as LN
import           XMonad.Layout.NoBorders      (smartBorders)
import qualified XMonad.Layout.ResizableTile  as LRT
import           XMonad.Util.Run              (spawnPipe)

import           GHC.IO.Handle                (Handle)
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import           System.Exit                  (ExitCode (..), exitWith)
import           System.IO                    (hPutStrLn)


import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces


projects :: [Project]
projects =
  []
  -- <>
  -- [ Project { projectName      = "scratch"
  --           , projectDirectory = "~/"
  --           , projectStartHook = Nothing
  --           }

  -- , Project { projectName      = "browser"
  --           , projectDirectory = "~/"
  --           , projectStartHook = Just $ do spawn "firefox"
  --                                          spawn "gvim"
  --           }
  -- ]

-- FIXME:
-- 1. Resolve problem with type -- import XMonad.Layout.LayoutModifier

main :: IO ()
main = do
  statusHandle <- spawnPipe $ "dzen2 -dock -w 720 -ta l" <> pcDzenStyle
  void $ spawnPipe $ "conky -c ~/.xmonad/conkyrc"
                  <> " | dzen2 -dock -x 720 -w 880 -ta r" <> pcDzenStyle

  xmonad $ dynamicProjects projects $ HMD.docks $ CD.desktopConfig
    { modMask = pcModMask
    , terminal = "xfce4-terminal"
    , borderWidth = 3
    , keys = \conf -> pcKeysFun conf
                      `M.union` (keys CD.desktopConfig) conf
                      `M.withoutKeys` pcKeysRemoveFun conf
    , workspaces = pcWorkspaces
    , logHook = pcLogHook statusHandle
    , layoutHook = pcLayoutHook
    , startupHook = HMD.docksStartupHook <+> pcStartupHook
    --, manageHook = HMD.manageDocks -- <+> myManageHook <+> manageHook defaultConfig
    , manageHook = HMD.manageDocks
               <+> manageHook CD.desktopConfig
    , handleEventHook = LF.fullscreenEventHook <+> HMD.docksEventHook
    }
  where
    -- XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
    -- pcLayoutHook :: (ModifiedLayout HMD.AvoidStruts (Choose Tall (Choose (Mirror Tall) Full))) Window
    -- FIXME: add type here (at least try)
    pcLayoutHook = HMD.avoidStruts $ smartBorders (tiled ||| full ||| mtiled)
      where
        -- default tile is Tall nmaster delta golden-ratio
        tiled  = LN.named "T" $ LRT.ResizableTall 1 (4 / 100) goldenRatio []
          where
            goldenRatio = 2 / (1 + toRational(sqrt(5)::Double))
        full   = LN.named "X" $ Full
        mtiled = LN.named "M" $ Mirror tiled

pcStartupHook :: X ()
pcStartupHook = do
  --setWMName "LG3D"
  --spawn "xset r rate 200 50"
  pure ()

pcWorkspaces :: [String]
pcWorkspaces = map (:[]) gen <> map (\c -> '\'':c:[]) gen
  where
    gen = ['u'] <> ['1' .. '9'] <> ['0', 'm', 'v']



pcDzenStyle :: String
pcDzenStyle = " -h 20 -fg '#777777' -bg '#222222' -fn arial:bold:size=11"

pcDzenPP :: HDL.PP
pcDzenPP = HDL.dzenPP
  { HDL.ppCurrent = HDL.dzenColor "#3399ff" "" . HDL.wrap " " " " . take 1
  , HDL.ppHidden  = HDL.dzenColor "#dddddd" "" . HDL.wrap " " " " . take 1
  , HDL.ppHiddenNoWindows = HDL.dzenColor "#777777" ""
                          . HDL.wrap " " " " . take 1
  , HDL.ppUrgent  = HDL.dzenColor "#ff0000" "" . HDL.wrap " " " "
  , HDL.ppSep     = "     "
  , HDL.ppLayout  = HDL.dzenColor "#aaaaaa" "" . HDL.wrap " " " "
  , HDL.ppTitle   = HDL.dzenColor "#ffffff" ""
                  . HDL.wrap " " " "
                  . HDL.shorten 64
                  . HDL.dzenEscape
  }


pcModMask :: KeyMask
pcModMask = mod4Mask

pcBrowser :: String
pcBrowser = "chromium"
--pcBrowser = "firefox"

pcBrowserIncognito :: String
pcBrowserIncognito = "chromium --incognito"
--pcBrowserIncognito = "firefox -private-window"

pcLock :: String
pcLock = "/home/czechow/bin/lock"

pcSuspend :: String
pcSuspend = "/home/czechow/bin/susp"

pcWsKeySyms :: [KeySym]
pcWsKeySyms = [xK_grave] <> [xK_1 .. xK_9] <> [xK_0, xK_minus, xK_equal]

upWS :: WindowSet -> WindowSet
upWS ws = case reverse $ SS.currentTag ws of
  ('\'':wsnameRev) -> SS.view (reverse wsnameRev) ws
  _                 -> ws

downAndMaybeAddWS :: X ()
downAndMaybeAddWS = maybeAddPrimWS >>= XMonad.windows . SS.view

shiftToUp :: WindowSet -> WindowSet
shiftToUp ws = case reverse $ SS.currentTag ws of
  ('\'':wsname) -> SS.shift (reverse wsname) ws
  _             -> ws

maybeAddPrimWS :: X WorkspaceId
maybeAddPrimWS = withWindowSet $ \(wset :: WindowSet) -> do
  let tag' :: WorkspaceId = SS.currentTag wset <> "'"
  case findWS wset tag' of
    Nothing -> appendWorkspace tag' >> pure tag'
    _ -> pure tag'

findWS :: WindowSet -> WorkspaceId -> Maybe WindowSpace
findWS ws wId =
  case filter (\SS.Workspace{SS.tag} -> tag == wId) $ SS.workspaces ws of
    [] -> Nothing
    v:_ -> Just v

shiftToDownAndMaybeAddWS :: X ()
shiftToDownAndMaybeAddWS = maybeAddPrimWS >>= XMonad.windows . SS.shift

pcKeysFun :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
pcKeysFun conf@XConfig{modMask} = M.fromList $
  [ ((modMask, ksym), XMonad.windows $ SS.view wsname)
  | (wsname, ksym) <- zip (XMonad.workspaces conf) pcWsKeySyms
  ]
  <>
  [ ((modMask .|. shiftMask, ksym), XMonad.windows $ SS.shift wsname)
  | (wsname, ksym) <- zip (XMonad.workspaces conf) pcWsKeySyms
  ]
  <>
  [ ((modMask, xK_Tab), CWS.toggleWS)  -- toggle last workspace (super-tab)
  , ((modMask, xK_Return), dwmpromote) -- swap focused with the master window

  , ((modMask, xK_Right), CWS.nextWS)
  , ((modMask, xK_Left), CWS.prevWS)
  , ((modMask, xK_Down), downAndMaybeAddWS)
  , ((modMask, xK_Up), XMonad.windows upWS)

  , ((modMask .|. shiftMask, xK_Right), CWS.shiftToNext)
  , ((modMask .|. shiftMask, xK_Left),  CWS.shiftToPrev)
  , ((modMask .|. shiftMask, xK_Down), shiftToDownAndMaybeAddWS)
  , ((modMask .|. shiftMask, xK_Up), XMonad.windows shiftToUp)

  , ((modMask .|. controlMask .|. shiftMask, xK_l), spawn pcLock)
  , ((modMask .|. controlMask .|. shiftMask, xK_s), spawn pcSuspend)
  , ((modMask .|. shiftMask, xK_n), spawn pcBrowser)
  , ((modMask .|. shiftMask .|. controlMask, xK_n), spawn pcBrowserIncognito)

  , ((modMask, xK_z), sendMessage LRT.MirrorShrink)
  , ((modMask, xK_a), sendMessage LRT.MirrorExpand)

  , ((modMask, xK_p), spawn "gmrun")
  , ((modMask, xK_o), spawn "/home/czechow/bin/passmenu --type")
  ]
  <> -- redefine xmonad restart/quit sequences
  [ ( (modMask .|. shiftMask, xK_q)
    , spawn "xmessage Use Super+Ctrl+Shift+q to quit xmonad")

  , ( (modMask .|. controlMask .|. shiftMask, xK_q)
    , io (exitWith ExitSuccess)) -- %! Quit xmonad
  , ((modMask .|. controlMask, xK_r)
    , spawn "if type xmonad; then \
            \  pkill conky; xmonad --recompile && xmonad --restart; \
            \else \
            \  xmessage xmonad not in \\$PATH: \"$PATH\"; \
            \fi")
  ]
  <> -- volume control keys
  [ ((0, XF86.xF86XK_AudioLowerVolume ), spawn "amixer set Master 10%-")
  , ((0, XF86.xF86XK_AudioRaiseVolume ), spawn "amixer set Master 10%+")
  , ((0, XF86.xF86XK_AudioMute        ), spawn "amixer set Master toggle \
                                               \&& amixer set Headphone on")
  ]
  <>
  [ ((modMask, xK_slash), switchProjectPrompt def)
  , ((modMask .|. shiftMask, xK_slash), shiftToProjectPrompt def)
  , ((modMask, xK_r), renameProjectPrompt def)
  , ((modMask, xK_BackSpace), removeWorkspace)
  ]

pcKeysRemoveFun :: XConfig Layout -> S.Set (ButtonMask, KeySym)
pcKeysRemoveFun _ = S.fromList []

pcLogHook :: Handle -> X ()
pcLogHook handle =
  HDL.dynamicLogWithPP $ pcDzenPP { HDL.ppOutput = hPutStrLn handle }


-- instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) => Default (XConfig a) where
--   def = XConfig
--     { XMonad.borderWidth        = borderWidth
--     , XMonad.workspaces         = workspaces
--     , XMonad.layoutHook         = layout
--     , XMonad.terminal           = terminal
--     , XMonad.normalBorderColor  = normalBorderColor
--     , XMonad.focusedBorderColor = focusedBorderColor
--     , XMonad.modMask            = defaultModMask
--     , XMonad.keys               = keys
--     , XMonad.logHook            = logHook
--     , XMonad.startupHook        = startupHook
--     , XMonad.mouseBindings      = mouseBindings
--     , XMonad.manageHook         = manageHook
--     , XMonad.handleEventHook    = handleEventHook
--     , XMonad.focusFollowsMouse  = focusFollowsMouse
--     , XMonad.clickJustFocuses   = clickJustFocuses
--     , XMonad.clientMask         = clientMask
--     , XMonad.rootMask           = rootMask
--     , XMonad.handleExtraArgs    = undefined
--     }
