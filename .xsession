#!/bin/sh

#
# ~/.xsession
#
# Executed by xdm/gdm/kdm at login
#

xrdb -merge .Xresources

trayer \
	--edge top --align right --SetDockType true --SetPartialStrut true \
	--expand true --width 5 --transparent true --tint 0x191900 --height 5 \
	--alpha 50 &

# xsetroot -solid midnightblue


nm-applet --sm-disable &
gnome-power-manager &
kmix &
kmixctrl &


exec xmonad

