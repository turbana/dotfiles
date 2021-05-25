import Control.Monad
import Data.Map (fromList, (!))
import Data.Maybe
import Data.List (find)
import qualified Data.Map        as M
import qualified XMonad.Config.Prime as X
import qualified XMonad.StackSet as W
import System.Directory
import System.Environment
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
import XMonad.Util.WorkspaceCompare

hostHome = "cyclone"

myWorkspaces = Prelude.map show [1..9]

myFont = "DejaVu Sans Mono-12"
-- myFont = "Nimbus Sans L Regular-11"
-- myFont = "Latin Modern Mono-12"

data Command = Editor | Dmenu | XMonad | LeftStatusBar | RightStatusBar | Calculator | OrgCapture
command cmd colors = case cmd of
  Editor -> "emacsclient -c -a ''"
  Dmenu  -> concat [
    "dmenu_run -b -p '>'"
    , " -nb '", (colors ! "base-4"), "'"
    , " -nf '", (colors ! "base+3"), "'"
    , " -sb '", (colors ! "yellow"), "'"
    , " -sf '", (colors ! "base-4"), "'"]
  XMonad -> "if type restart-xmonad; then xmonad-restart; "
            ++ "else xmessage xmonad-restart not in \\$PATH: \"$PATH\"; fi"
  LeftStatusBar -> concat [
    "dzen2 -y 1060 -x 0 -w 1420 -ta l -h 24 -xs 1 -dock"
    , " -fg '", (colors ! "base+3"), "'"
    , " -bg '", (colors ! "base-3"), "'"
    , " -fn '", myFont, "'"]
  RightStatusBar -> "$HOME/.xmonad/dzen2-right-bar.sh"


loadColors filename = do
  home <- getHomeDirectory
  x <- readFile $ (expandHome home filename)
  return . fromList . map (toPair . words) . lines $ x
  where
    expandHome home ('~':xs) = home ++ xs
    expandHome home path     = path
    toPair xs = (xs !! 0, xs !! 1)


main = do
  etc <- getEnv "ETC"
  colors <- loadColors $ etc ++ "/colors/current"
  writeFile "/dev/null" $ show colors -- XXX we have to expand colors for some reason
  dzenLeftBar <- spawnPipe $ command LeftStatusBar colors
  xmonad $ docks $ myConfig dzenLeftBar colors `additionalKeysP` myKeys colors

myConfig h colors = def {
    terminal           = "gnome-terminal",
    startupHook        = myStartupHook colors,
    borderWidth        = 2,
    normalBorderColor  = colors ! "base-3",
    focusedBorderColor = colors ! "blue",
    manageHook         = manageHooks,
    layoutHook         = layoutHooks,
    logHook            = logHooks h colors
  }

myStartupHook colors = do
  spawn $ command RightStatusBar colors
  setDefaultCursor xC_left_ptr

myKeys colors =
  [("M-j", windows W.focusDown)
  ,("M-k", windows W.focusUp)
  ,("S-M-j", windows W.swapDown)
  ,("S-M-k", windows W.swapUp)
  ,("<Pause>", namedScratchpadAction myScratchPads "calc")
  ,("<F1>", namedScratchpadAction myScratchPads "orgCap")
  ,("M-p", spawn $ command Dmenu colors)
  ,("M-o", spawn $ command Editor colors)
  ,("C-M-x r", spawn $ command XMonad colors)
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
  NS "calc" "gnome-terminal --role=calculator --title=calc -x $ETC/bin/calc"
  (role =? "calculator")
  (customFloating $ W.RationalRect (3/8) (1/4) (1/4) (1/2)),

  NS "orgCap" "emacsclient -c -F '(quote (name . \"OrgCapture\"))' -e '(org-capture)'"
  (title =? "OrgCapture")
  (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  ]
  where role = stringProperty "WM_WINDOW_ROLE"


layoutHooks =
  avoidStruts $
  smartBorders $
  onHost hostHome (tall ||| full) $              -- Home layouts (below this are work layouts)
  onWorkspace "2" (Mirror tallLarge ||| full) $
  reflectVert tile ||| full
  where
    full       = Full
    tall       = Tall      nmaster delta 0.50
    tallLarge  = Tall      nmaster delta 0.70
    tile       = StackTile nmaster delta 0.57
    tileMany   = StackTile nmaster delta 0.25
    nmaster    = 1
    delta      = 0.03
    golden     = toRational (2/(1 + sqrt 5 :: Double))


manageHooks = composeAll [
    namedScratchpadManageHook myScratchPads
    -- manageDocks,
    ,className =? "Pidgin"  --> doShift "2"
    ,className =? "Pidgin"  --> doF avoidMaster
    ,wmName =? "dzen slave" --> doIgnore
  ]
  where
    wmName = stringProperty "WM_NAME"
    avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
    avoidMaster = W.modify' $ \c -> case c of
      W.Stack t [] (r:rs) ->  W.Stack t [r] rs
      otherwise           -> c


logHooks h colors = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ def {
   ppCurrent           = dzenColor yellow bg . (++ underline)
  ,ppVisible           = dzenColor fg bg
  ,ppHidden            = dzenColor grey bg
  ,ppHiddenNoWindows   = const ""
  ,ppUrgent            = dzenColor red bg
  ,ppTitle             = dzenColor fg bg . dzenEscape
  ,ppLayout            = const ""
  ,ppExtras            = [-- drawBorder
                         ]
  ,ppWsSep             = " "
  ,ppSep               = dzenColor grey bg "  |  "
  ,ppOutput            = hPutStrLn h . ("^p(5)" ++) -- put a padding on the left
  ,ppSort              = getSortByXineramaRule
}
  where
    cyan   = colors ! "cyan"
    bg     = colors ! "base-3"
    fg     = colors ! "base+3"
    yellow = colors ! "yellow"
    grey   = colors ! "base+1"
    red    = colors ! "red"
    -- border = colors ! "base-2"
    ignoreBg = wrap "^ib(1)" "^ib(0)"
    -- drawBorder =
    --   return $ Just $ dzenColor border bg $ ignoreBg
    --   "^pa(0)^ro(1420x1-0-11)"
    underline = ignoreBg "^r(10x2-10+9)"
