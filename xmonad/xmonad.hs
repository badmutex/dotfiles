
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.IO
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
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


myTerminal = "terminator"
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
    Full |||
    spiral (6/7))



myManageHook = manageHook defaultConfig

myGSConfig = defaultGSConfig

myKeys conf@(XConfig {XMonad.modMask = modm}) =
    M.fromList [
           ((modm               , xK_Right), nextWS)
         , ((modm               , xK_Left) , prevWS)
         , ((modm .|. shiftMask , xK_Right), shiftToNext >> nextWS)
         , ((modm .|. shiftMask , xK_Left) , shiftToPrev >> prevWS)
         , ((modm               , xK_g)    , goToSelected myGSConfig)
         , ((modm .|. shiftMask , xK_b)    , io (randomBackground>>return ()))
         ]


defaults =
    let cfg = defaultConfig
    in cfg {
    -- simple stuff
    terminal           = myTerminal
  , modMask            = myModMask

  -- hooks, layouts
  , layoutHook = smartBorders $ myLayout

  -- key bindings
  , keys = myKeys <+> keys cfg

  }


startCompositing = spawnPID "xcompmgr"
randomBackground = spawnPID "feh --no-fehbg --randomize --bg-scale $HOME/Backgrounds"


-- | convert minutes into microseconds
minutes :: Int -> Int
minutes m = m * 10^6 * 60

periodicRandomBackground interval =
    forkIO $ forever $ do
      randomBackground
      threadDelay interval


main = do
  startCompositing
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

