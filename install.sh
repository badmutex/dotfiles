#!/usr/bin/env bash

cfgs=(
    config/autostart/Dropbox.desktop
    config/autostart/xfce4-volumed.desktop
    config/autostart/XCompMGR.desktop
    gitconfig
    nixpkgs
    oh-my-zsh
    synergy.conf
    tmux.conf
    xmonad
    zshrc
)

with-home() {
    pushd $HOME >/dev/null 2>&1
    eval $@
    popd >/dev/null 2>&1
}

cd $(dirname $0)

dotfilesdir=$PWD

for cfg in ${cfgs[@]}; do

    cd $HOME
    dst=.$cfg
    src=$dotfilesdir/$cfg

    if test -e $dst; then
	echo "WARNING $dst already exists, creating backup"
	mv -v $dst $dst.backup.$(date "+%F-%T")
    fi

    dstdir=$(dirname $dst)
    test -d "$dstdir" || mkdir -vp "$dstdir"
    cd "$dstdir"
    dst=$(basename $dst)
    ln -sv $src $dst

done
