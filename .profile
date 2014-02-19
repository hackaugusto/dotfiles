[[ -f /etc/profile ]] && . /etc/profile                         # XDG variables
[[ -s $HOMW/.xbindkeys ]] && xbindkeysrc &                      # multimedia keys
[[ -s $HOME/.nvm/nvm.sh ]] && . $HOME/.nvm/nvm.sh               # This loads NVM

command -v ssh-agent >/dev/null 2>&1 && [[ -z $SSH_AGENT_PID ]] && eval $(ssh-agent) >/dev/null 2>&1

[[ ! -z $DISPLAY && -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap      # remaps Caps to Ctrl
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
