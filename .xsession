#!/bin/sh

#
# ~/.xsession
#
# Executed by xdm/gdm/kdm at login
#

xrdb -merge .Xresources

trayer \
	--edge top --align right --SetDockType true --SetPartialStrut true \
	--expand true --width 5 --transparent true --tint 0x000000 --height 5 \
	--alpha 0 &


mpd ~/.mpd/mpd.conf &
nm-applet --sm-disable &
gnome-power-manager &
kmix &
kmixctrl &
$HOME/.xmonad/random-background.hs > $HOME/.xmonad/random-background.log &



exec xmonad

