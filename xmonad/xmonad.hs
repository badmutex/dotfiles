
import XMonad

import XMonad.Actions.CycleWS
import XMonad.Config.Desktop

import XMonad.Hooks.ManageDocks

import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed

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

myLayout = avoidStruts (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full
    )

myKeys conf@(XConfig {XMonad.modMask = modm}) =
    M.fromList [
           ((modm               , xK_Right), nextWS)
         , ((modm               , xK_Left) , prevWS)
         , ((modm .|. shiftMask , xK_Right), shiftToNext >> nextWS)
         , ((modm .|. shiftMask , xK_Left) , shiftToPrev >> prevWS)
         ]

main = do
  let cfg = desktopConfig
  xmonad cfg {
         terminal = myTerminal
       , modMask = myModMask
       , keys = myKeys <+> keys cfg
       , layoutHook = smartBorders $ myLayout
       }
