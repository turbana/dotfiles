import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import System.Posix.Unistd
import XMonad.Layout.OnHost
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import System.IO

hostHome = "cyclone"

main = do
  dzenLeftBar <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  xmonad $ defaultConfig {
    borderWidth        = 3,
    normalBorderColor  = "#000000",
    focusedBorderColor = "#009900",
    layoutHook         = layoutHooks,
    manageHook         = manageHooks,
    logHook            = logHooks dzenLeftBar
  }


layoutHooks =
  avoidStruts $
  desktopLayoutModifiers $
  onHost hostHome (tiled ||| Full) $              -- Home layouts (below this are work layouts)
  onWorkspace "2" (Mirror tiledLarge ||| Full) $
  reflectVert $ Mirror tiled ||| Full
  where
    tiled       = Tall 1 (3/100) (1/2)
    tiledLarge  = Tall 1 (3/100) (70/100)

      
manageHooks = composeAll [
    className =? "Icedove" --> doShift "2",
    className =? "Pidgin"  --> doShift "2",
    className =? "Pidgin"  --> doF avoidMaster,
    manageDocks
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
