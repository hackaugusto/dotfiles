[[ -f ~/.profile ]] && . ~/.profile
[[ -f ~/.bashrc ]] && . ~/.bashrc
bin vex && eval "$(vex --shell-config bash)"
