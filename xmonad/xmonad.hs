import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import System.IO
import System.Posix.Unistd
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Config
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Combo
import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.OnHost
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.StackTile
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation --XXX
import XMonad.ManageHook
import XMonad.Util.Cursor
import XMonad.Util.CustomKeys
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run

hostHome = "cyclone"

myWorkspaces = map show [1..9]

dzenBackground = "#262626"

myCommand "editor" = "emacsclient -c -a '' --eval '(spacemacs/home)'"
myCommand "dmenu"  = "dmenu_run -b -nb '#333333' -nf '#eeeeee' -sb '#afaf00' -sf '#000000' -p '>'"
myCommand "xmonad" = "if type xmonad; then killall dzen2; xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
myCommand "left-status-bar" = "$HOME/.xmonad/dzen2-left-bar.sh"
myCommand "right-status-bar" = "$HOME/.xmonad/dzen2-right-bar.sh"

main = do
  dzenLeftBar <- spawnPipe $ myCommand "left-status-bar"
  xmonad $ docks $ myConfig dzenLeftBar `additionalKeysP` myKeys_

myConfig h = def {
    terminal           = "gnome-terminal",
    startupHook        = spawn $ myCommand "right-status-bar",
    borderWidth        = 2,
    normalBorderColor  = "#000000",
    focusedBorderColor = "#67e671",
    manageHook         = manageHooks,
    layoutHook         = layoutHooks,
    logHook            = logHooks h
  }

myKeys_ =
  [("M-j", windows W.focusDown)
  ,("M-k", windows W.focusUp)
  ,("S-M-j", windows W.swapDown)
  ,("S-M-k", windows W.swapUp)
  ,("<Pause>", namedScratchpadAction myScratchPads "calc")
  ,("<F1>", namedScratchpadAction myScratchPads "orgCap")
  ,("M-p", spawn $ myCommand "dmenu")
  ,("M-o", spawn $ myCommand "editor")
  ,("C-M-x r", spawn $ myCommand "xmonad")
  ,("c-M-x s", sendMessage ToggleStruts)
  ]
  ++
  -- bind M-# and S-M-# to navigate or move a window to workspace #
  [ (otherModMasks ++ "M-" ++ key, action tag)
  | (tag, key)  <- zip myWorkspaces myWorkspaces
  , (otherModMasks, action) <- [ ("", windows . W.greedyView) -- or W.view
                               , ("S-", windows . W.shift)]
  ]


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
    className =? "Pidgin"  --> doShift "2",
    className =? "Pidgin"  --> doF avoidMaster
  ]
  where
    avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    avoidMaster = W.modify' $ \c -> case c of
      W.Stack t [] (r:rs) ->  W.Stack t [r] rs
      otherwise           -> c


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
