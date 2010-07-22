#!/usr/bin/env sh

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


nm-applet --sm-disable &
kmix &
kmixctrl &

$HOME/.init.sh



exec xmonad

