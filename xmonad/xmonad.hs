
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (forever)
import           System.Exit
import           System.IO
import           System.Posix.Process         (getProcessID)
import           System.Posix.Signals         (sigTERM, signalProcess)

import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Types

import           XMonad

import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Volume

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

import           XMonad.Util.EZConfig         (additionalKeys)
import           XMonad.Util.Run              (spawnPipe)

import           XMonad.Config.Kde

import qualified Data.Map                     as M
import qualified XMonad.StackSet              as W


myTerminal = "terminology"
myModMask = mod4Mask


-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    threeCol |||
    tabbed shrinkText tabConfig |||
    Full
    )
    where threeCol = ThreeColMid 1 (3/100) (1/2)



myManageHook = composeAll [
                 className =? "kmix" --> doFloat
               , className =? "plasma-desktop" --> doFloat
               , className =? "Plasma-desktop" --> doFloat
               , className =? "plasmashell" --> doFloat
               ]

myGSConfig = defaultGSConfig


rofi = "rofi -show run -fg '#505050' -bg '#000000' -hlfg '#ffb964' -hlbg '#000000' -hide-scrollbar"


myKeys conf@(XConfig {XMonad.modMask = modm}) =
    M.fromList [
           ((modm               , xK_p)    , spawn rofi)
         , ((modm               , xK_Right), nextWS)
         , ((modm               , xK_Left) , prevWS)
         , ((modm .|. shiftMask , xK_Right), shiftToNext >> nextWS)
         , ((modm .|. shiftMask , xK_Left) , shiftToPrev >> prevWS)
         , ((modm               , xK_g)    , goToSelected myGSConfig)
         , ((modm               , xK_b)    , spawn "google-chrome-stable")
         , ((noModMask, xF86XK_MonBrightnessUp)    , spawn "xbacklight +20")
         , ((noModMask, xF86XK_MonBrightnessDown)  , spawn "xbacklight -20")
         , ((noModMask, xF86XK_AudioRaiseVolume) , spawn "amixer set 'Master' 1%+")
         , ((noModMask, xF86XK_AudioLowerVolume) , spawn "amixer set 'Master' 1%-")
         , ((noModMask, xF86XK_AudioMute) , spawn "amixer set 'Master' toggle")
         , ((mod1Mask .|. controlMask, xK_l), spawn "xdg-screensaver lock")
         ]


defaults =
    let cfg = defaultConfig
    in cfg {
    -- simple stuff
    terminal           = myTerminal
  , modMask            = myModMask

  -- hooks, layouts
  , layoutHook = smartBorders $ myLayout

  , manageHook = manageDocks <+> myManageHook <+> manageHook cfg


  -- key bindings
  , keys = myKeys <+> keys cfg

  }

main = do
  xmobarProc <- spawnPipe "xmobar ~/.xmobar.hs"
  xmonad $  defaults {
    logHook = dynamicLogWithPP $ xmobarPP {
      ppOutput = \s -> hPutStrLn xmobarProc s
    , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
    , ppSep = "    "
    }
  , manageHook = manageHook kde4Config <+> manageDocks <+> myManageHook
  }

