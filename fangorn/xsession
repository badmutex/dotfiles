#!/usr/bin/env bash

start-if-cmd-exists () {
    which $1 && $* &
}

with-dbus () {
    echo "Running $@" 1>&2
    start-if-cmd-exists dbus-launch --exit-with-session $*
}

with-dbus \
    trayer \
    --edge top --align right --SetDockType true --SetPartialStrut true \
    --expand true --width 10 --height 14 \
    --transparent true --alpha 0 --tint 0x000000

with-dbus dropbox
with-dbus nm-applet
with-dbus kmix

exec xmonad
