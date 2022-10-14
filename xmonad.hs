import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.DwmPromote
import XMonad.Hooks.ManageDocks
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders
import XMonad.Util.Ungrab
import XMonad.Layout.Spacing
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen
import XMonad.Layout.MultiToggle.Instances
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Actions.SinkAll
import XMonad.Actions.CopyWindow
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.WindowBringer
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile


myLayouts = tiled |||
            StackTile 1 (3/100) (1/2)
  where
     tiled   = ResizableTall nmaster delta ratio[]
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myTitleColor     = "white"      -- color of window title
myTitleLength    = 80           -- truncate window title to this length
myCurrentWSColor = "#03070D"       -- color of active workspace
myVisibleWSColor = "white"      -- color of inactive workspace
myUrgentWSColor  = "red"        -- color of workspace with 'urgent' window
myHiddenNoWindowsWSColor = "white"

--WORKSPACES
xmobarEscape = concatMap doubleLts
    where doubleLts '<' = "<"
          doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
    where
               clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" | (i,ws) <- zip [1, 2, 3, 4, 5, 6, 7, 8, 9] l, let n = i ]

myKeys :: [([Char], X ())]
myKeys =
  -- Commands
    [ ("M-<Return>",     spawn $ "st")
    , ("M-q",            kill)
    , ("M--"  ,          spawn "pamixer --allow-boost -d 5")
    , ("M-=",            spawn "pamixer --allow-boost -i 5")
    , ("M-p", unGrab *>  spawn "rofi -show run")
    , ("<Print>",        spawn "~/scripts/screenshot.sh")
    , ("M-c",            spawn "alacritty -e ~/scripts/clipboard.sh")
    , ("M-S-<Return>",   spawn "pcmanfm-qt")
    , ("M-s",            spawn "~/scripts/search.sh")
    , ("<Scroll_lock>",  spawn "~/scripts/recordvideo.sh")
    , ("<Pause>",        spawn "~/scripts/endvideo.sh")
    , ("M-e e",          spawn "emacsclient -c -e \"(set-frame-parameter (selected-frame) 'alpha-background 0.85)\"")
    , ("M-x 1",          spawn "echo 10 | sudo tee /sys/class/backlight/acpi_video0/brightness")
    , ("M-x 2",          spawn "echo 15 | sudo tee /sys/class/backlight/acpi_video0/brightness")
    , ("M-x 3",          spawn "echo 20 | sudo tee /sys/class/backlight/acpi_video0/brightness")
    , ("M-x t",           spawn "~/scripts/task.sh")
    -- From Imported Libraries
    , ("M-b",            sendMessage ToggleStruts)
    , ("M-S-=",          incWindowSpacing 1)
    , ("M-S--",          decWindowSpacing 1)
    , ("M-f",            gotoMenu)
    , ("M-C-f",          bringMenu)        
    , ("M-S-.",          shiftToNext)
    , ("M-S-,",          shiftToPrev)
    , ("M-M1-j",         sendMessage MirrorShrink)
    , ("M-M1-k",         sendMessage MirrorExpand)
    , ("M-,",            DO.moveTo Prev HiddenNonEmptyWS)
    , ("M-.",            DO.moveTo Next HiddenNonEmptyWS)    
    , ("M-m",            dwmpromote)
    , ("M-S-f",          sinkAll)
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe  "xmobar"
   -- xmproc <- spawnPipe "ls"
    xmonad $ ewmhFullscreen $ ewmh $ docks $ def
--spacingWithEdge 5 
      { layoutHook      =    smartSpacing 8 $ avoidStruts $ smartBorders $ myLayouts
      , focusedBorderColor = myFocusedBorderColor
      , startupHook     =    setWMName "LG3D"
      , borderWidth     =    myBorderWidth
      , normalBorderColor =  myNormalBorderColor
      , workspaces      =    myWorkspaces
      , manageHook      =    composeAll
                            [ className =? "TrayCalendar" --> doIgnore
                            , className =? "Galculator" --> doFloat
                            , className =? "Pcmanfm" --> doSink
                            , role =? "page-info" --> doFloat
                            , role =? "GtkFileChooserDialog" --> doSink
                            ]

      , modMask         =    mod4Mask
      , logHook         =    dynamicLogWithPP $ xmobarPP
                             { ppOutput  = hPutStrLn xmproc
                            , ppTitle   = xmobarColor "#12afcf" "" . shorten 35
                            , ppCurrent = wrap "<box type=Bottom color=#12afcf>""</box>"
                            , ppVisible = wrap """"
                            , ppHidden  = wrap "<fc=#12afcf,#12afcf>""</fc>"
                            , ppHiddenNoWindows  = wrap "<fc=#FFFFFF,#FFFFFF>""</fc>"
                            , ppUrgent  = xmobarColor myUrgentWSColor ""
                            , ppSep     = ""
                            , ppWsSep   = ""                
                            , ppLayout  = const "<fc=#FFFFFF,#FFFFFF>|</fc> "
}               } `additionalKeysP` myKeys
                        where role = stringProperty "WM_WINDOW_ROLE"



myBorderWidth = 3
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#12afcf"
