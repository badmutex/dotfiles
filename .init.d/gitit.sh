#!/usr/bin/env sh



GITIT_HOME=$HOME/gitit


cd $GITIT_HOME

( if ! flock -xn 200 ; then
      echo "Gitit already running"
      exit 0
  fi

  screen -d -m -S gitit gitit
) 200>$GITIT_HOME/gitit.lock
