#!/bin/sh



# trayer \
# 	--edge top --align right --SetDockType true --SetPartialStrut true \
# 	--expand true --width 5 --transparent true --tint 0x000000 --height 5 \
# 	--alpha 0 &


# nm-applet --sm-disable &
# kmix &
# kmixctrl &
# xscreensaver &
# $HOME/.init.sh

xcompmgr -c &

(while true; do feh --no-fehgb --randomize --bg-scale $HOME/Backgrounds; sleep 30m; done) &

exec xmonad

