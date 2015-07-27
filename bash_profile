# .bash_profile

######################################################################
# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

######################################################################
# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin

export PATH

######################################################################
# keychain

preloaded_ssh_keys=(
    id_rsa
)

eval $(keychain --eval --agents ssh ${preloaded_ssh_keys[@]})

######################################################################
# 
# Local Variables:
# mode: sh
# End:
