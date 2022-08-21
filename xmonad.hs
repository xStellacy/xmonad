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

myLayouts = tiled
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100


myTitleColor     = "white"      -- color of window title
myTitleLength    = 80           -- truncate window title to this length
myCurrentWSColor = "#14bbd8"    -- color of active workspace
myVisibleWSColor = "white"      -- color of inactive workspace
myUrgentWSColor  = "red"        -- color of workspace with 'urgent' window
myHiddenNoWindowsWSColor = "white"


--WORKSPACES
xmobarEscape = concatMap doubleLts
    where doubleLts '<' = "<"
          doubleLts x = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
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
    , ("M-S-<Return>",   spawn "pcmanfm")
    , ("M-s",            spawn "firefox")
    , ("<Scroll_lock>",  spawn "~/scripts/recordvideo.sh")
    , ("<Pause>",        spawn "~/scripts/endvideo.sh")
    , ("M-e e",          spawn "emacsclient -c -e \"(set-frame-parameter (selected-frame) 'alpha-background 0.85)\"")
    , ("M-x p",          spawn "keepass -auto-type-selected")
    , ("M-x 1",          spawn "echo 10 | sudo tee /sys/class/backlight/acpi_video0/brightness")
    , ("M-x 2",          spawn "echo 15 | sudo tee /sys/class/backlight/acpi_video0/brightness")
    , ("M-x 3",          spawn "echo 20 | sudo tee /sys/class/backlight/acpi_video0/brightness")
    -- From Imported Libraries
    , ("M-b",            sendMessage ToggleStruts)
    , ("M-f",            gotoMenu)
    , ("M-C-f",          bringMenu)        
    , ("M-S-=",          incWindowSpacing 4)
    , ("M-S--",          decWindowSpacing 4)
    , ("M-S-.",          shiftToNext)
    , ("M-S-,",          shiftToPrev)
    , ("M-,",            DO.moveTo Prev HiddenNonEmptyWS)
    , ("M-.",            DO.moveTo Next HiddenNonEmptyWS)    
    , ("M-m",            dwmpromote)
    , ("M-S-f",          sinkAll)
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe  "xmobar"
    xmonad $ ewmhFullscreen $ ewmh $ docks $ def
      { layoutHook      =    smartSpacing 2 $ avoidStruts $ smartBorders $ myLayouts
      , focusedBorderColor = myFocusedBorderColor
      , startupHook = setWMName "LG3D"
      , borderWidth     =    myBorderWidth
      , normalBorderColor =  myNormalBorderColor
      , workspaces      =    myWorkspaces
      , manageHook      =    composeAll
                            [ className =? "TrayCalendar" --> doIgnore
                            , className =? "Galculator" --> doFloat
                            , role =? "page-info" --> doFloat
                            , role =? "GtkFileChooserDialog" --> doSink
                            ]

      , modMask         =    mod4Mask
      , logHook         =    dynamicLogWithPP $ xmobarPP
      
                            { ppOutput  = hPutStrLn xmproc
                            , ppTitle   = xmobarColor "white" "" . shorten 35
                            , ppCurrent = xmobarColor myCurrentWSColor "" . wrap "<fc=#14bbd8><</fc>"">"
                            , ppVisible = xmobarColor myVisibleWSColor "" . wrap """"
                            , ppHidden  = wrap """"
                            , ppHiddenNoWindows = xmobarColor myHiddenNoWindowsWSColor ""
                            , ppUrgent  = xmobarColor myUrgentWSColor ""
                            , ppSep     = "  "
                            , ppWsSep   = "  "                
                            , ppLayout  = const "<fc=#bc1fFF>|</fc>"
}               } `additionalKeysP` myKeys
                        where role = stringProperty "WM_WINDOW_ROLE"

myBorderWidth = 3
--myFocusedBorderColor = "#1f0fc8"
myFocusedBorderColor = "red"
myNormalBorderColor = "grey"
