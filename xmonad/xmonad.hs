
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.IO
import System.Exit
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess, sigTERM)

import Graphics.X11.ExtraTypes.XF86

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Volume

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myTerminal = "konsole"
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
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full
    )



myManageHook = composeAll [
                 className =? "kmix" --> doFloat
               , className =? "Kmix" --> doFloat
               , className =? "plasma-desktop" --> doFloat
               , className =? "Plasma-desktop" --> doFloat
               , className =? ".xfce4-panel-wrapped" --> doFloat
               , className =? ".xfce4-panel-wrapped" --> doFloat
               ]

myGSConfig = defaultGSConfig

myKeys conf@(XConfig {XMonad.modMask = modm}) =
    M.fromList [
           ((modm               , xK_Right), nextWS)
         , ((modm               , xK_Left) , prevWS)
         , ((modm .|. shiftMask , xK_Right), shiftToNext >> nextWS)
         , ((modm .|. shiftMask , xK_Left) , shiftToPrev >> prevWS)
         , ((modm               , xK_g)    , goToSelected myGSConfig)
         -- , ((0,   xF86XK_AudioLowerVolume) , lowerVol 3 >> return ())
         -- , ((0,   xF86XK_AudioRaiseVolume) , raiseVol 3 >> return ())
         -- , ((0,   xF86XK_AudioMute       ) , toggleMute >> return ())
         , ((modm .|. shiftMask , xK_b)    , io (randomBackground>>return ()))
         ]
    -- where lowerVol = lowerVolumeChannels ["Master"]
    --       raiseVol = raiseVolumeChannels ["Master"]


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

randomBackground = spawnPID "feh --no-fehbg --randomize --bg-scale $HOME/Backgrounds"

-- | convert minutes into microseconds
minutes :: Int -> Int
minutes m = m * 10^6 * 60

periodicRandomBackground interval =
    forkIO $ forever $ do
      randomBackground
      threadDelay interval


main = do
  -- startCompositing
  -- bgPID <- periodicRandomBackground (minutes 30)
  spawn "xfdesktop --quit"
  periodicRandomBackground (minutes 30)
  xmobarProc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $  defaults {
    logHook = dynamicLogWithPP $ xmobarPP {
      ppOutput = hPutStrLn xmobarProc
    , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
    , ppSep = "    "
    }
  , manageHook = manageDocks <+> myManageHook
  }

