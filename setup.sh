#!/usr/bin/env bash

cfgs=(
    synergy.conf
    tmux.conf
    xmonad
    oh-my-zsh
    zshrc
)

with-home() {
    pushd $HOME >/dev/null 2>&1
    eval $@
    popd >/dev/null 2>&1
}

cd $(dirname $0)

dotfilesdir=$(basename $PWD)

cd $HOME

for cfg in ${cfgs[@]}; do

    dst=.$cfg

    if test -e $dst; then
	echo "WARNING $dst already exists, creating backup"
	mv -v $dst $dst.backup.$(date "+%F-%T")
    fi

    ln -sv $dotfilesdir/$cfg $dst

done
