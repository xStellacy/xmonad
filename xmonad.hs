import XMonad
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

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

myTerminal      = "st"
myBorderWidth = 2
-- myWorkspaces         = ["  \62059 ","\61564","\61598", "\58923","\58923", "\58923", "\61441", "\61441", "\61441"]

main :: IO ()
main = do
--    dbus <- D.connectSession
--    -- Request access to the DBus name
--    D.requestName dbus (D.busName_ "org.xmonad.Log")
--        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

--     xmonad . withUrgencyHook NoUrgencyHook =<< statusBar myBar myPP toggleStrutsKey -- (ewmh myConfig){ logHook = dynamicLogWithPP (myLogHook dbus)}
--    xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
    xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
--
--myLogHook :: D.Client -> PP
--myLogHook dbus = def { ppOutput = dbusOutput dbus }
--
--dbusOutput :: D.Client -> String -> IO ()
--dbusOutput dbus str = do
--    let signal = (D.signal objectPath interfaceName memberName) {
--            D.signalBody = [D.toVariant $ UTF8.decodeString str]
--        }
--    D.emit dbus signal
--  where
--    objectPath = D.objectPath_ "/org/xmonad/Log"
--    interfaceName = D.interfaceName_ "org.xmonad.Log"
--    memberName = D.memberName_ "Update"



-- layoutHook = spacingWithEdge 10 $ myLayoutHook
myFocusedBorderColor = "#6f0194"
myNormalBorderColor = "#4e5f5f"

-- Command to launch the bar.
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#04eb0a" "" . wrap "<" ">" }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


-- Main configuration, override the defaults to your liking.
myConfig = def { modMask = mod4Mask
   -- , handleEventHook    = fullscreenEventHook
    --, handleEventHook    = handleEventHook def <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook
    , focusedBorderColor = myFocusedBorderColor
--    , workspaces         = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , borderWidth = myBorderWidth
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
    , ("M-S-<Space>", windows copyToAll)        
    


    , ("M-m", dwmpromote)
    ]
myLayout = spacingWithEdge 5 $ StackTile 1 (3/100) (1/2) ||| tiled ||| Mirror tiled ||| Full ||| Grid ||| magnifier (Tall 1 (3/100) (1/2))
   where
     tiled   = Tall nmaster delta ratio
     nmaster = 1      -- Default number of windows in the master pane
     ratio   = 1/2    -- Default proportion of screen occupied by master pane
     delta   = 3/100  -- Percent of screen to increment by when resizing panes

