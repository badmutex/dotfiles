# Path to your oh-my-zsh installation.

cache-oh-my-zsh() {
    set -x
    local today="$(date +%F)"
    local cachedir=$XDG_CONFIG_HOME/oh-my-zsh/zshrc_cache
    local todays_file="$cachedir/$today"
    if ! test -f "$todays_file"; then
        rm -rf "$cachedir"
        mkdir -p "$cachedir"
        local oh_my_zsh="$(nix-env -q --out-path badi-packages | cut -d' ' -f3)/share/oh-my-zsh"
        echo "$oh_my_zsh" > "$todays_file"
    fi
    cat "$todays_file"
    set +x
}

export ZSH=$(cache-oh-my-zsh)

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussel"
ZSH_THEME="blinks"
ZSH_THEME="junkfood"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="dd.mm.yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    # haskell
    cabal

    # docker
    docker

    # url/web/json
    encode64 urlencode jsontools

    # git
    git git-extras git-flow github

    # python
    python autopep8 pep8 pip #virtualenvwrapper

    # shell
    screen

    # systemd
    systemd

    # virtualization
    vagrant

)

# User configuration

export PATH=$HOME/bin:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

test -f $HOME/.shell_shortcuts && source $HOME/.shell_shortcuts

# Enable more extensive tab completion
autoload -U compinit
compinit

# Tab completion on both ends
setopt completeinword

# Change the definition of "word"
autoload select-word-style
select-word-style bash

# haskell `stack` tool autocompletion
# http://docs.haskellstack.org/en/stable/shell_autocompletion.html
if [ $(command -v stack) ]; then
    autoload -U +X compinit && compinit
    autoload -U +X bashcompinit && bashcompinit
    eval "$(stack --bash-completion-script stack)"
fi

# enable direnv
# http://direnv.net/
if [ $(command -v direnv) ]; then
    eval "$(direnv hook zsh)"
fi


test -f /etc/zlogin && source /etc/zlogin
test -f $HOME/dotfiles/desktop-setup/keychain.sh && source $HOME/dotfiles/desktop-setup/keychain.sh

######################################################################

repl() {
    local arg="${1:=$HOME/dotfiles/nixpkgs/nixpkgs.nix}"
    nix-repl $arg
}

## Local Variables:
## mode: shell-script
## End:
