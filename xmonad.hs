import XMonad
import Dzen
import XMonad.Hooks.DynamicLog
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Actions.DwmPromote
import XMonad.Layout.Spacing
import XMonad.Actions.Submap
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.StackTile
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab
import System.IO
import XMonad.Actions.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.WithAll
import XMonad.Actions.WindowBringer
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Actions.CopyWindow
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.CycleWS
import Data.Char (isSpace)


import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

myTerminal      = "alacritty"
myBorderWidth = 1

myWorkspaces = ["www","edit","vnc","files","mail","chat","media","chat","other"]
-- Custom theme colors
red       = "#da3e37" --"#ed6666"
green     = "#6feabb" --"#83be49"
white     = "#f1e3e2" --"#e7e6e6"
lightGrey = "#cfcfcf"
grey      = "#646464"
darkGrey  = "#444444"
black     = "#000000"

myLogHook h = dynamicLogWithPP $ myPrettyPrinter h
myPrettyPrinter h = dzenPP
  {
    ppOutput          = hPutStrLn h
  , ppCurrent         = dzenColor black red . pad
  , ppHidden          = dzenColor white black . pad . clickable myWorkspaces . trimSpace
  , ppHiddenNoWindows = dzenColor grey black . pad . clickable myWorkspaces . trimSpace
  , ppUrgent          = dzenColor black green . pad . clickable myWorkspaces . trimSpace . dzenStrip
  , ppWsSep           = " "
  , ppSep             = " | "
  , ppTitle           = (" " ++) . dzenColor red black . shorten 120 . dzenEscape
  , ppLayout          = dzenColor white black . pad .
                        (\x -> case x of
                          "SimplestFloat"              -> "Float"
                          "SmartSpacing 2 Tall"        -> "Tall"
                          "SmartSpacing 2 Mirror Tall" -> "Mirror"
                          "SmartSpacing 2 IM"               -> "IM"
                          _                            -> x
                        )
  }

-- Wraps a workspace name with a dzen clickable action that focusses that workspace.
clickable workspaces workspace = clickableExp workspaces 1 workspace

clickableExp [] _ ws = ws
clickableExp (ws:other) n l | l == ws = "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()"
                            | otherwise = clickableExp other (n+1) l

-- Trims leading and trailing white space.
trimSpace = f . f
    where f = reverse . dropWhile isSpace

myDzenFont = "DejaVu Sans Mono:pixelsize=10:Bold"

-- Workspace dzen bar
myWorkDzen = DzenConf {
    x_position = Just 0
  , y_position = Just 0
  , width      = Just 1810
  , height     = Just 22
  , alignment  = Just LeftAlign
  , font       = Just myDzenFont
  , fg_color   = Just lightGrey
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

-- Music dzen bar
myMusicDzen = DzenConf {
    x_position = Just 0
  , y_position = Just 0
  , width      = Just 880
  , height     = Just 22
  , alignment  = Just LeftAlign
  , font       = Just myDzenFont
  , fg_color   = Just red
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

-- Date dzen bar
myDateDzen = DzenConf {
    x_position = Just 1230
  , y_position = Just 0
  , width      = Just 160
  , height     = Just 22
  , alignment  = Just Centered
  , font       = Just myDzenFont
  , fg_color   = Just "#da3e37"
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

-- Volume dzen bar
myVolumeDzen = DzenConf {
    x_position = Just 1170
  , y_position = Just 0
  , width      = Just 60
  , height     = Just 22
  , alignment  = Just Centered
  , font       = Just myDzenFont
  , fg_color   = Just "#da3e37"
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

-- System information dzen bar
mySysInfoDzen = DzenConf {
    x_position = Just 0
  , y_position = Just 0
  , width      = Just 160
  , height     = Just 22
  , alignment  = Just RightAlign
  , font       = Just myDzenFont
  , fg_color   = Just red
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

------------------------------------------------------------------------
-- Startup hook
-- By default, do nothing.
--

main :: IO ()
main = do
    xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
    




myFocusedBorderColor = "#ed1b24"
--myFocusedBorderColor = "#44689b"
myNormalBorderColor = "#12e4db"
--myNormalBorderColor = "#4e5f5f"

-- Command to launch the bar.
--myBar = "xmobar -x 0 | xmobar -x 1"
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
--myPP = xmobarPP { ppCurrent = xmobarColor "#44689b" "" . wrap "<" ">" }
myPP = xmobarPP { ppCurrent = xmobarColor "#b14c47" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


-- Main configuration, override the defaults to your liking.
myConfig = def { modMask = mod4Mask
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor = myNormalBorderColor
    , borderWidth = myBorderWidth
    , workspaces = myWorkspaces
    , layoutHook = myLayout}
      `additionalKeysP`
    [ ("M-<Return>", spawn $ myTerminal)
    , ("M-q", kill)
    , ("M-p", unGrab *> spawn "rofi -show run"        )
    , ("M--"  , spawn "pamixer --allow-boost -d 5")
    , ("M-=", spawn "pamixer --allow-boost -i 5")
    , ("<Print>", spawn $ "cd ~/Pictures/Screenshots && import $(date '+%y%m%d-%H%M-%S').png")
    , ("<Scroll_Lock>", spawn $ "~/cfgs/scripts/video.sh")
    , ("<Pause>", spawn $ "~/cfgs/scripts/video.sh end")
    , ("M-c", spawn "clipmenu")
    , ("M-s", spawn "~/cfgs/scripts/search.sh")
    , ("M-S-f", sinkAll)
    , ("M-g", gotoMenu)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-S-=", incWindowSpacing 4)
    , ("M-S--", decWindowSpacing 4)
    , ("M-]", sendMessage MagnifyMore)
    , ("M-[", sendMessage MagnifyLess)
    , ("M-s", windows copyToAll)
    , ("M-S-s", killAllOtherCopies)   
    , ("M-,", DO.moveTo Prev HiddenNonEmptyWS)   
    , ("M-.", DO.moveTo Next HiddenNonEmptyWS)   
    , ("M-S-.", shiftToNext)
    , ("M-S-,", shiftToPrev)
    , ("M-<Tab>",  nextScreen)
    , ("M-e e", spawn $ "emacsclient -c -e \"(set-frame-parameter (selected-frame) 'alpha-background 0.85)\"")
    , ("M-m", dwmpromote)
    ]
myLayout = tiled ||| Mirror tiled ||| StackTile 1 (3/100) (1/2) ||| Full ||| Grid ||| magnifier (Tall 1 (3/100) (1/2))
--myLayout = spacingWithEdge 5 tiled ||| Mirror tiled ||| StackTile 1 (3/100) (1/2) ||| Full ||| Grid ||| magnifier (Tall 1 (3/100) (1/2))
   where
     tiled   = Tall nmaster delta ratio
     nmaster = 1      -- Default number of windows in the master pane
     ratio   = 1/2    -- Default proportion of screen occupied by master pane
     delta   = 3/100  -- Percent of screen to increment by when resizing panes
