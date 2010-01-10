#!/usr/bin/env sh


INIT_DIR=$HOME/.init.d

for f in $INIT_DIR/*; do
	if [ -x $f ]; then
		$f
	fi
done
