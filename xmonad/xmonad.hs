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
import XMonad.Util.Cursor
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Util.NamedScratchpad

import XMonad.ManageHook

import XMonad.Layout.StackTile
import XMonad.Layout.Grid

import XMonad.Layout.Combo
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation --XXX
import XMonad.Layout.NoBorders

import XMonad.Actions.PhysicalScreens

hostHome = "cyclone"

main = do
  dzenLeftBar <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  xmonad $ defaultConfig {
    terminal           = "gnome-terminal",
    startupHook        = setDefaultCursor xC_left_ptr,
    borderWidth        = 2,
    normalBorderColor  = "#202020",
    focusedBorderColor = "#009900",
    layoutHook         = layoutHooks,
    manageHook         = manageHooks,
    logHook            = logHooks dzenLeftBar,
    keys               = customKeys emptyKeys myKeys
  }

emptyKeys XConfig {modMask = modm} = []

myKeys conf@(XConfig {modMask = modm}) =
  [ ((mod1Mask,               xK_k    ), windows W.focusUp  )
  , ((mod1Mask,               xK_j    ), windows W.focusDown)
  , ((mod1Mask .|. shiftMask, xK_k    ), windows W.swapUp)
  , ((mod1Mask .|. shiftMask, xK_j    ), windows W.swapDown)
  , ((mod1Mask .|. shiftMask, xK_l    ), sendMessage Shrink)
  , ((mod1Mask .|. shiftMask, xK_h    ), sendMessage Expand)
  , ((mod1Mask,               xK_l    ), sendMessage $ Go R)
  , ((mod1Mask,               xK_h    ), sendMessage $ Go L)
  , ((mod1Mask,               xK_k    ), sendMessage $ Go U)
  , ((mod1Mask,               xK_j    ), sendMessage $ Go D)
  , ((0       ,               xK_Pause), namedScratchpadAction myScratchPads "calc")
  , ((0       ,               xK_F1   ), namedScratchpadAction myScratchPads "orgCap")
  , ((mod1Mask,               xK_p    ), spawn "dmenu_run -b -nb '#333333' -nf '#eeeeee' -sb '#afaf00' -sf '#000000' -p '>'")
  , ((mod1Mask .|. controlMask, xK_e  ), spawn "emacsclient -c -a '' --eval '(spacemacs/home)'")
  , ((mod1Mask .|. controlMask, xK_q  ), spawn "if type xmonad; then killall dzen2; xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  ]
  ++
  -- mod-{q,w,e} to switch screens
  [((m .|. mod1Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_q, xK_w, xK_e] [2,0,1]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myScratchPads = [
  NS "calc" "gnome-terminal --role=calculator --title=calc -x ~/.etc/bin/calc"
  (role =? "calculator")
  (customFloating $ W.RationalRect (3/8) (1/4) (1/4) (1/2)),

  NS "orgCap" "emacsclient -c -F '(quote (name . \"OrgCapture\"))' -e '(org-capture)'"
  (title =? "OrgCapture")
  (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  ]
  where role = stringProperty "WM_WINDOW_ROLE"


layoutHooks =
  avoidStruts $
  desktopLayoutModifiers $
  smartBorders $
  onHost hostHome (tall ||| Full) $              -- Home layouts (below this are work layouts)
  onWorkspace "2" (Mirror tallLarge ||| Full) $
  reflectVert tile ||| Full
  where
    tall       = Tall      nmaster delta 0.50
    tallLarge  = Tall      nmaster delta 0.70
    tile       = StackTile nmaster delta 0.57
    tileMany   = StackTile nmaster delta 0.25
    nmaster    = 1
    delta      = 0.03
    golden     = toRational (2/(1 + sqrt 5 :: Double))


manageHooks = composeAll [
    namedScratchpadManageHook myScratchPads,
    manageDocks,
    className =? "Pidgin"  --> doShift "2",
    className =? "Pidgin"  --> doF avoidMaster
  ]
  where
    avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    avoidMaster = W.modify' $ \c -> case c of
      W.Stack t [] (r:rs) ->  W.Stack t [r] rs
      otherwise           -> c


myXmonadBar = "$HOME/.xmonad/dzen2-left-bar.sh"
myStatusBar = "$HOME/.xmonad/dzen2-right-bar.sh"
dzenBackground = "#222222"

logHooks h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ defaultPP {
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
