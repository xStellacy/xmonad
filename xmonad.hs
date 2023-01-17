import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.DwmPromote (dwmpromote)
import XMonad.Hooks.ManageDocks (ToggleStruts (..),  avoidStruts, docks)
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders (noBorders, smartBorders, hasBorder)
import XMonad.Layout.Spacing (smartSpacing, incWindowSpacing, decWindowSpacing)
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppLayout, ppOutput, ppTitle, ppCurrent, ppVisible, ppHidden, ppHiddenNoWindows, ppUrgent, ppSep, ppWsSep, filterOutWsPP, wrap, shorten, xmobarColor, xmobarPP)
import System.IO
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Hooks.EwmhDesktops (ewmh, addEwmhWorkspaceSort, ewmhFullscreen)
import XMonad.Hooks.SetWMName (setWMName)
import qualified XMonad.StackSet as W (greedyView, swapDown, swapUp, focusDown, focusUp, RationalRect (..))
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.StackTile (StackTile (..))
import XMonad.Util.NamedScratchpad (NamedScratchpad (..), defaultFloating, nonFloating, scratchpadWorkspaceTag, namedScratchpadManageHook, namedScratchpadAction)
import XMonad.Hooks.WindowSwallowing (swallowEventHook)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.WorkspaceCompare (getSortByIndex, filterOutWs)
import XMonad.Util.Scratchpad (scratchpadFilterOutWorkspace)
import XMonad.Util.DynamicScratchpads (makeDynamicSP, spawnDynamicSP)
import XMonad.Actions.FloatKeys (keysResizeWindow, keysMoveWindow) 
import XMonad.Actions.NoBorders (toggleBorder)

myLayouts = (noBorders tiled |||
            StackTile 1 (3/100) (1/2))
  where
     tiled       = ResizableTall nmaster delta ratio[]
     nmaster     = 1
     ratio       = 1/2
     delta       = 3/100

myTitleColor             =    "white"   -- color of window title
myTitleLength            =    80        -- truncate window title to this length
myCurrentWSColor         =    "#03070D" -- color of active workspace
myVisibleWSColor         =    "white"   -- color of inactive workspace
myUrgentWSColor          =    "red"     -- color of workspace with 'urgent' window
myHiddenNoWindowsWSColor =    "white"

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
  -- COMMANDS
    [ ("M-<Return>",                spawn "st" <> withFocused toggleBorder)
    , ("<XF86AudioRaiseVolume>",    spawn "sndioctl output.level=+.1") -- Volume up
    , ("<XF86AudioLowerVolume>",    spawn "sndioctl output.level=-.1") -- Volume down

    , ("<F3>",                      spawn "sndioctl output.level=+.1") -- Volume up
    , ("<F2>",                      spawn "sndioctl output.level=-.1") -- Volume down
 --   , ("<XF86AudioMute>",           spawn "pamixer -t") -- Volume mute
    , ("<XF86MonBrightnessUp>",     spawn "xbacklight -inc 20") -- Brightness up
    , ("<XF86MonBrightnessDown>",   spawn "xbacklight -dec 20") -- Brightness down
    , ("M-p",                       spawn "exit")
    , ("M-r",                       spawn "rofi -config .cache/wal/colors-rofi-dark.rasi -show run")
    , ("<Print>",                   spawn "~/bin/screenshot.sh")
    , ("M-x w",                     spawn "~/bin/randomwallpaper.sh")
    , ("M-x r",                     spawn "~/bin/removewallpaper.sh")
    , ("M-x c",                     spawn "~/bin/xmobarwal.sh")
    , ("<Scroll_lock>",             spawn "~/bin/recordvideo.sh")
    , ("M-<Print>",                 spawn "~/bin/recordvideo.sh")
    , ("M-S-<Print>",               spawn "~/bin/endvideo.sh")
    , ("M-x t",                     spawn "~/bin/task.sh")
    -- FROM IMPORTED LIBRARIES
    , ("M-b",                       sendMessage ToggleStruts)
    , ("M-S-=",                     incWindowSpacing 1)
    , ("M-S--",                     decWindowSpacing 1)
    , ("M-S-.",                     shiftToNext)
    , ("M-S-,",                     shiftToPrev)
    , ("M-,",                       windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1)
    , ("M-.",                       windows . W.greedyView =<< findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1)    
    , ("M-m",                       dwmpromote)
    , ("M-S-f",                     sinkAll)
    , ("M-[",                       namedScratchpadAction scratchpads "Htop")
    , ("M-e",                       namedScratchpadAction scratchpads "Emacs")
    , ("M-f",                       namedScratchpadAction scratchpads "Firefox")
    , ("M-S-<Return>",              namedScratchpadAction scratchpads "Pcmanfm")
    , ("M-t",                       namedScratchpadAction scratchpads "Terminal")
    , ("M-u",                       namedScratchpadAction scratchpads "Keepass")
    , ("M-x n",                     withFocused $ makeDynamicSP "dyn1")
    , ("M-n",                       spawnDynamicSP "dyn1")

    , ("M-C-k",                     withFocused (keysMoveWindow (0,-20)))            -- Move window up
    , ("M-C-j",                     withFocused (keysMoveWindow (0,20)))             -- Move window down
    , ("M-C-h",                     withFocused (keysMoveWindow (-20,0)))            -- Move window left
    , ("M-C-l",                     withFocused (keysMoveWindow (20,0)))             -- Move window right
                                     
    , ("M-M1-k",                    withFocused (keysResizeWindow (0,-20) (0,0)))    -- Resize window up
    , ("M-M1-j",                    withFocused (keysResizeWindow (0,30) (0,0)))     -- Resize window down
    , ("M-M1-h",                    withFocused (keysResizeWindow (-20,0) (0,0)))    -- Resize window left
    , ("M-M1-l",                    withFocused (keysResizeWindow (20,0) (0,0)))     -- Resize window right

   , ("M-g",                        withFocused toggleBorder)
    , ("M-j",                       windows W.focusDown <> withFocused toggleBorder )
    , ("M-k",                       windows W.focusUp   <> withFocused toggleBorder )
    , ("M-S-k",                     windows W.swapUp    <> withFocused toggleBorder )
    , ("M-S-j",                     windows W.swapDown  <> withFocused toggleBorder )
    , ("M-l",                       sendMessage Expand  <> withFocused toggleBorder )
    , ("M-h",                       sendMessage Shrink  <> withFocused toggleBorder )
    , ("M-q",                       kill)
    , ("M-<Tab>",                   toggleWS' ["NSP"])
    ]

getSortByIndexNoSP =   fmap (.scratchpadFilterOutWorkspace) getSortByIndex
toggleOrViewNoSP   =   toggleOrDoSkip ["NSP"] W.greedyView
myFilter           =   filterOutWs [scratchpadWorkspaceTag]

main :: IO ()
main = do
    xmproc <- spawnPipe  "DBUS_SYSTEM_BUS_ADDRESS=unix:path=/run/dbus/system_bus_socket DBUS_SESSION_BUS_ADDRESS=unix:path=/run/dbus/system_bus_socket xmobar -x 0 ~/.xmobarrc"
    xmonad $ ewmhFullscreen $ ewmh $ docks $ addEwmhWorkspaceSort (pure myFilter) $ desktopConfig
      { layoutHook      =    smartSpacing 8 $ avoidStruts $ smartBorders $ myLayouts
      , handleEventHook =    swallowEventHook (className =? "St") (return True)
      , focusedBorderColor = myFocusedBorderColor
      , startupHook     =    setWMName "LG3D"
      , borderWidth     =    myBorderWidth
      , normalBorderColor =  myNormalBorderColor
      , workspaces      =    myWorkspaces
      , manageHook      =    composeAll
                            [ className =? "TrayCalendar" -->    doIgnore
                            , className =? "Galculator" -->      doFloat
                            , className =? "Pcmanfm" -->         doSink
                            , className =? "St" -->              hasBorder False
                            , className =? "Firefox" -->         hasBorder False
                            , className =? "htop" -->            doRectFloat (W.RationalRect 0 0 1 1)
                            , className =? "terminal" -->        doRectFloat (W.RationalRect 0 0 1 1)
                            , role =? "page-info" -->            doFloat
                            , role =? "GtkFileChooserDialog" --> doSink
                            , isDialog -->                       doCenterFloat
                            ] <+> namedScratchpadManageHook scratchpads
      , modMask         =    mod4Mask
      , logHook         =    dynamicLogWithPP $ filterOutWsPP [scratchpadWorkspaceTag] $ xmobarPP
                             { ppOutput  = hPutStrLn xmproc
                            , ppTitle   = wrap "                                                                                                                                                              " "" . shorten 0
                            , ppCurrent          = wrap "<box type=Bottom width=2 color=#bac>""</box>"
                            , ppVisible          = wrap """"
                            , ppHidden           = wrap "<fc=#bac,#for>""</fc>"
                            , ppHiddenNoWindows  = wrap "<fc=#FFFFFF,#FFFFFF>""</fc>"
                            , ppSep              = ""
                            , ppWsSep            = ""                
                            , ppLayout           = const "<fc=#bac,#FFFFFF>|</fc> "
                            , ppUrgent           = xmobarColor myUrgentWSColor ""
}               } `additionalKeysP` myKeys
                        where role = stringProperty "WM_WINDOW_ROLE"

myBorderWidth = 4
myNormalBorderColor  = "#for"
myFocusedBorderColor = "#bac"

scratchpads = [
    NS "Htop" "st -c htop htop"       (className =? "htop")            defaultFloating ,
    NS "Firefox" "firefox"            (className =? "firefox-default") nonFloating ,
    NS "Emacs" "~/bin/emacsclient.sh" (className =? "Emacs")           nonFloating ,
    NS "Terminal" "st -c terminal"    (className =? "terminal")        defaultFloating ,
    NS "Keepass" "keepassxc"          (className =? "KeePassXC")       nonFloating,
    NS "Pcmanfm" "QT_STYLE_OVERRIDE=kvantum dbus-launch pcmanfm-qt" (className =? "pcmanfm-qt") nonFloating
    ] where role = stringProperty "WM_WINDOW_ROLE"
