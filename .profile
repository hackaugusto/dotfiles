# Force sourcing of /etc/profile (correctly set XDG variables for su and sudo)
[[ -f /etc/profile ]] && . /etc/profile
[[ ! -z $DISPLAY && -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

# command -v ssh-agent > /dev/null 2>&1 && eval $(ssh-agent) && ssh-add
command -v ssh-agent >/dev/null 2>&1 && [[ -z $SSH_AGENT_PID ]] && eval $(ssh-agent) >/dev/null 2>&1

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
