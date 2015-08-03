if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# git
alias glog='git log --oneline --graph --decorate=short'
alias gst='git status'
alias gl='git pull'
alias gp='git push'
alias gd='git diff'
alias gdc='git diff --cached'
alias gc='git commit'
alias gco='git checkout'
