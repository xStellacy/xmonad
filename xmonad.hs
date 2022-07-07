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

myLayouts = avoidStruts (smartSpacing 2 (smartBorders (tiled)))
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myKeys :: [([Char], X ())]
myKeys =
  -- Commands
    [ ("M-<Return>",     spawn $ "alacritty")
    , ("M-q",            kill)
    , ("M--"  ,          spawn "pamixer --allow-boost -d 5")
    , ("M-=",            spawn "pamixer --allow-boost -i 5")
    , ("M-p", unGrab *>  spawn "rofi -show run")
    , ("<Print>",        spawn $ "cd ~/Pictures/Screenshots && import $(date '+%y%m%d-%H%M-%S').png")
    , ("<Scroll_Lock>",  spawn $ "~/cfgs/scripts/video.sh")
    , ("<Pause>",        spawn $ "~/cfgs/scripts/video.sh end")
    , ("M-c",            spawn "CM_HISTLENGTH=999999 CM_LAUNCHER=dmenu clipmenu")
    , ("M-d",            spawn "dolphin")
    , ("M-e e",          spawn $ "emacsclient -c -e \"(set-frame-parameter (selected-frame) 'alpha-background 0.85)\"")
    -- From Imported Libraries
    , ("M-b",            sendMessage ToggleStruts)
    , ("M-S-=",          incWindowSpacing 4)
    , ("M-S--",          decWindowSpacing 4)
    , ("M-S-.",          shiftToNext)
    , ("M-S-,",          shiftToPrev)
    , ("M-,",            DO.moveTo Prev HiddenNonEmptyWS)
    , ("M-.",            DO.moveTo Next HiddenNonEmptyWS)    
    , ("M-m",            dwmpromote)
    , ("M-S-f",          sinkAll)
    , ("M-s",            windows copyToAll)    
    ]

main :: IO ()
main = do
    xmproc <- spawnPipe  "xmobar"
    xmonad $ fullscreenSupport $ docks $ def
      { layoutHook      = myLayouts
      , focusedBorderColor = myFocusedBorderColor
      , borderWidth     = myBorderWidth
      , normalBorderColor =  myNormalBorderColor
      , handleEventHook = fullscreenEventHook
      , manageHook      = fullscreenManageHook
      , modMask         = mod4Mask
      , logHook = dynamicLogWithPP $ xmobarPP
      
                { ppOutput  = hPutStrLn xmproc
                , ppTitle   = xmobarColor "white" "" . shorten 100
                , ppSep     = xmobarColor "grey" "" " | "
                , ppCurrent = xmobarColor "#00989b" "" . wrap "<" ">"
                , ppHidden  = xmobarColor "#44689b" ""
                , ppHiddenNoWindows = xmobarColor "white" ""
                , ppLayout          = const ""
}               } `additionalKeysP` myKeys

myBorderWidth = 2
myFocusedBorderColor = "red"
myNormalBorderColor = "#44689b"
