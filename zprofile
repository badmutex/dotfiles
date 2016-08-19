######################################################################
# add Nixpkgs if not already done so

profile=$HOME/.nix-profile/etc/profile.d/nix.sh
if [ -f $profile ] && [ -z $NIX_PATH ]; then
    source $profile
fi

######################################################################

export EDITOR=emacs

if [ $(command -v most >/dev/null) ]; then
    export MANPAGER=most
fi


# 
# Local Variables:
# mode: shell-script
# End:
