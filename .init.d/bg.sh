#!/usr/bin/env sh


SOURCE=$HOME/.xmonad

(
	if ! flock -xn 200; then
		echo "Random background already randomized!"
		exit 0;
	fi

	screen -d -m -S bg $SOURCE/random-background.hs
) 200>$SOURCE/random-background.log
