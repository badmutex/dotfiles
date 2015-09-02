######################################################################
# add Nixpkgs if not already done so

profile=$HOME/.nix-profile/etc/profile.d/nix.sh
if [ -f $profile ] && [ -z $NIX_PATH ]; then
    source $profile
fi

######################################################################

export EDITOR=emacs


# 
# Local Variables:
# mode: shell-script
# End:
