######################################################################
# add Nixpkgs if not already done so

profile=$HOME/.nix-profile/etc/profile.d/nix.sh
if [ -f $profile ] && [ -z $NIX_PATH ]; then
    source $profile
fi

######################################################################
# keychain

preloaded_ssh_keys=(
    id_rsa
)

eval $(keychain --eval --agents ssh ${preloaded_ssh_keys[@]})

######################################################################

export EDITOR=emacs


# 
# Local Variables:
# mode: shell-script
# End:
