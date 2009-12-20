import XMonad hiding ((|||))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Accordion
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowArranger



main = do
  xmonad =<< xmobar
           defaultConfig {
             modMask     = mod4Mask
           , terminal    = myTerminal
	   , borderWidth = 2
           , workspaces  = myWorkspaces
           , layoutHook  = myLayoutHook
           , manageHook  = myManageHook
           , startupHook = myStartupHook
           }

myTerminal = "urxvt -bg darkgrey -fg green -cr green -vb +sb -bc -tr -tint black -sh 10"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9]

myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.4

myLayoutHook = mine
    where default' = layoutHook defaultConfig
          mine = avoidStruts $ windowArrange $ 
                 noBorders Full ||| Mirror tiled ||| Accordion ||| tiled
              where tiled = Tall 1 (3/100) (1/2)

myManageHook = composeOne [ isFullscreen -?> doFullFloat ]

myStartupHook = setWMName "LG3D"