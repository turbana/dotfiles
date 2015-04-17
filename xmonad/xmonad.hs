import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.Posix.Unistd
import XMonad.Layout.OnHost
import XMonad.Util.Run
import XMonad.Util.CustomKeys
import XMonad.Hooks.DynamicLog
import System.IO

import XMonad.Util.NamedScratchpad

import XMonad.Layout.StackTile
import XMonad.Layout.Grid

import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation --XXX

import XMonad.Actions.PhysicalScreens

--import XMonad.Hooks.EwmhDesktops
--import XMonad.Hooks.SetWMName

hostHome = "cyclone"

main = do
  dzenLeftBar <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  xmonad $ defaultConfig {
    terminal           = "gnome-terminal",
    borderWidth        = 2,
    normalBorderColor  = "#202020",
    focusedBorderColor = "#009900",
    layoutHook         = layoutHooks,
--    manageHook         = manageHooks,
    manageHook         = manageHooks <+> manageDocks <+> namedScratchpadManageHook myScratchPads,
--    manageHook         = namedScratchpadManageHook myScratchPads,
    logHook            = logHooks dzenLeftBar,
    keys               = customKeys emptyKeys myKeys
  }

emptyKeys XConfig {modMask = modm} = []

myKeys conf@(XConfig {modMask = modm}) =
  [ ((mod1Mask,               xK_j    ), windows W.focusUp  )
  , ((mod1Mask,               xK_k    ), windows W.focusDown)
  , ((mod1Mask .|. shiftMask, xK_j    ), windows W.swapUp)
  , ((mod1Mask .|. shiftMask, xK_k    ), windows W.swapDown)
  , ((mod1Mask .|. shiftMask, xK_l    ), sendMessage Shrink)
  , ((mod1Mask .|. shiftMask, xK_h    ), sendMessage Expand)
  , ((mod1Mask,               xK_l    ), sendMessage $ Go R)
  , ((mod1Mask,               xK_h    ), sendMessage $ Go L)
  , ((mod1Mask,               xK_j    ), sendMessage $ Go U)
  , ((mod1Mask,               xK_k    ), sendMessage $ Go D)
--  , ((mod1Mask,               xK_comma), sendMessage (IncMasterN (-1)))
--  , ((mod1Mask,              xK_period), sendMessage (IncMasterN 1))
--  , ((0       ,               xK_Pause), spawn "calc")
  , ((0       ,               xK_Pause), namedScratchpadAction myScratchPads "calc")
  , ((0       ,               xK_F1   ), namedScratchpadAction myScratchPads "orgCap")
  , ((mod1Mask,               xK_p    ), spawn "dmenu_run -m 0 -b -nb '#555555' -nf '#eeeeee' -sb '#003399' -sf '#ffffff' -p '>'")
  , ((mod1Mask .|. controlMask, xK_e  ), spawn "emacsclient -c -a ''")
--  , ((mod1Mask,               xK_F2   ), spawn "/home/iclark/src/ewu/whd/update-timetracking.sh")
  , ((mod1Mask .|. controlMask, xK_q  ), spawn "if type xmonad; then killall dzen2; xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
--  , ((mod1Mask,               xK_a    ), scratchPad)
  ]
  ++
  -- mod-{q,w,e} to switch screens
  [((m .|. mod1Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_q, xK_w, xK_e] [2,0,1]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myScratchPads = [NS "calc" "gnome-terminal -t calculator -e calc"
                 (title =? "calculator")
                 (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)),

                 NS "orgCap" "emacsclient -c -F '(quote (name . \"OrgCapture\"))' -e '(org-capture)'"
                 (className =? "OrgCapture")
                 (customFloating $ W.RationalRect (1/2) (1/2) (1/2) (1/2))
                ]


layoutHooks =
  avoidStruts $
  desktopLayoutModifiers $
  onHost hostHome (tall ||| Full) $              -- Home layouts (below this are work layouts)
  onWorkspace "2" (Mirror tallLarge ||| Full) $
  reflectVert tile ||| Full
--  reflectVert tile ||| reflectVert tileMany ||| windowNavigation combined ||| Full
  where
    tall       = Tall      nmaster delta golden
    tallLarge  = Tall      nmaster delta 0.70
    tile       = StackTile nmaster delta 0.57
    tileMany   = StackTile nmaster delta 0.25
    --combined   = combineTwo (Mirror $ TwoPane 0.03 0.75) (reflectVert Grid) (tile)
    nmaster    = 1
    delta      = 0.03
    golden     = toRational (2/(1 + sqrt 5 :: Double))
    half       = 0.5
    third      = 0.33


manageHooks = composeOne [
    className =? "Pidgin"  -?> doShift "2",
    className =? "Pidgin"  -?> doF avoidMaster
  ]
  where
    avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    avoidMaster = W.modify' $ \c -> case c of
      W.Stack t [] (r:rs) ->  W.Stack t [r] rs
      otherwise           -> c


old_manageHooks = composeAll [
    namedScratchpadManageHook myScratchPads,
    className =? "Pidgin"  --> doShift "2",
    className =? "Pidgin"  --> doF avoidMaster--,
--    manageDocks
  ]
  where
    avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    avoidMaster = W.modify' $ \c -> case c of
      W.Stack t [] (r:rs) ->  W.Stack t [r] rs
      otherwise           -> c


myXmonadBar = "$HOME/.xmonad/dzen2-left-bar.sh"
myStatusBar = "$HOME/.xmonad/dzen2-right-bar.sh"
dzenBackground = "#222222"

logHooks h = dynamicLogWithPP $ defaultPP {
  ppCurrent           = dzenColor "#ffffff" dzenBackground,
  ppVisible           = dzenColor "#ffffff" dzenBackground,
  ppHidden            = dzenColor "#cccc33" dzenBackground,
  ppHiddenNoWindows   = dzenColor "#666666" dzenBackground,
  ppUrgent            = dzenColor "#ff0000" dzenBackground,
  ppTitle             = dzenColor "#cccccc" dzenBackground . dzenEscape,
  ppLayout            = dzenColor dzenBackground dzenBackground,
  ppWsSep             = " ",
  ppSep               = "  |  ",
  ppOutput            = hPutStrLn h
}
