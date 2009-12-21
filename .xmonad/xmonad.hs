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

import XMonad.Config.Kde

import qualified XMonad.StackSet as W -- sho shift and float windows



main = do
  xmonad =<< xmobar
           kde4Config {
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
          mine     = avoidStruts . windowArrange $ 
                     noBorders Full ||| Mirror tiled ||| tiled
          tiled    = Tall 1 (3/100) (1/2)

myManageHook = composeOne [ isFullscreen -?> doFullFloat ] 
               <+> manageHook kde4Config
               <+> hooks
    where
      hooks     = composeAll . concat $
                  [ [ manageDocks ]
                  , [ className =? c --> doFloat | c <- myFloats ]
                  ]

      myFloats  = ["plasma-desktop", "Plasma-desktop", "plasma"]


myStartupHook = setWMName "LG3D"
