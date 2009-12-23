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

import XMonad.Util.Run (spawnPipe)

import XMonad.Config.Kde

import qualified XMonad.StackSet as W -- sho shift and float windows


import System.IO



main = do
  xmproc <- spawnPipe "xmobar"
  xmonad =<< xmobar 
           myConfig {
             modMask           = mod4Mask
           , terminal          = myTerminal
	   , borderWidth       = 1
           , focusFollowsMouse = False
           , workspaces        = map show [1..9]
           , layoutHook        = myLayoutHook
           , manageHook        = myManageHook
           , startupHook       = myStartupHook
           , logHook           = myXmobarPP xmproc
           }

myXmobarPP xmproc = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc
                                                , ppTitle = xmobarColor "white" "" . shorten 60
                                                }

myConfig = defaultConfig

myTerminal = "urxvt -bg darkgrey -fg green -cr green -vb +sb -bc -tr -tint black -sh 10"

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..9]

myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.4


myLayoutHook = mine
    where mine     = avoidStruts $ windowArrange $ 
                     noBorders Full ||| Mirror tiled ||| tiled
          tiled    = Tall 1 (3/100) (1/2)

myManageHook = manageHook myConfig
               <+> hooks
               <+> manageDocks
               <+> composeOne [ isFullscreen -?> doFullFloat ] 
    where
      hooks     = composeAll . concat $
                  [ [ className =? c --> doFloat | c <- myFloats ]
                  ]

      myFloats  = [ "plasma-desktop", "Plasma-desktop", "plasma", "kmix", "Kmix"
                  , "vlc", "Vlc"
                  ]


myStartupHook = setWMName "LG3D"
