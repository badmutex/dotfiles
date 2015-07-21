-- default desktop configuration for Fedora

import System.Posix.Env (getEnv)
import Data.Maybe (maybe)
import qualified Data.Map as M

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Kde
import XMonad.Config.Xfce
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns


myTerminal = "mate-terminal"

myModMask = mod4Mask

myLayout = smartBorders $ avoidStruts (
    tall |||
    ThreeCol 1 (3/100) (1/2) |||
    ThreeColMid 1 (3/100) (1/2) |||
    Mirror tall |||
    tabbed shrinkText tabConfig |||
    Full
    )
  where tall = Tall 1 (3/100) (1/2)
  	col3 = ThreeCol 1 (3/100) (1/2)
        tabConfig = defaultTheme {
	    activeBorderColor = "#7C7C7C",
            activeTextColor = "#CEFFAC",
            activeColor = "#000000",
            inactiveBorderColor = "#7C7C7C",
            inactiveTextColor = "#EEEEEE",
            inactiveColor = "#000000"
	}


myKeys conf@(XConfig {XMonad.modMask = modm}) =
    M.fromList [
           ((modm               , xK_Right), nextWS)
         , ((modm               , xK_Left) , prevWS)
         , ((modm .|. shiftMask , xK_Right), shiftToNext >> nextWS)
         , ((modm .|. shiftMask , xK_Left) , shiftToPrev >> prevWS)
         ]


main = do
     session <- getEnv "DESKTOP_SESSION"
     let cfg = maybe desktopConfig desktop session
     xmonad $ cfg {
     	        modMask = myModMask
              , terminal = myTerminal
	      , layoutHook = myLayout
              , startupHook = setWMName "LG3D"
	      , keys = myKeys <+> keys cfg
	      }

desktop "gnome" = gnomeConfig
desktop "kde" = kde4Config
desktop "xfce" = xfceConfig
desktop "xmonad-mate" = gnomeConfig
desktop _ = desktopConfig
