#!/usr/bin/env bash

export XDG_CONFIG_HOME=$HOME/.config


[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

urxvtd -q -f -o

xsetroot -cursor_name left_ptr &

# See for description of properties:
# https://www.x.org/wiki/Development/Documentation/PointerAcceleration/

# acceleration profile
xinput set-prop \
       'Kensington Kensington Slimblade Trackball' \
       'Device Accel Profile' \
       2  # polynomial

# # pointer acceleration
# xinput set-prop \
    #        'Kensington Kensington Slimblade Trackball' \
    #        'Device Accel Constant Deceleration' \
    #        0.5

xinput set-prop \
       'Kensington Kensington Slimblade Trackball' \
       'Device Accel Adaptive Deceleration' \
       1



xsettingsd &

compton -b
# ~/.xsession-background


xscreensaver -no-capture-stderr -no-splash &
parcellite &
nm-applet &
pa-applet &
blueman-applet &

eval $(keychain --eval --agents ssh,gpg --noask)

emacs --daemon &

[[ -f ~/.xsession-custom ]] && source ~/.xsession-custom

exec xmonad

