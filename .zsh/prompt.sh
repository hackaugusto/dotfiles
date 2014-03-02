git_filestatus() {
    zgit_isgit      || return 1
    zgit_inworktree || return 1

    local output

    if zgit_hasunmerged; then
        output='%F{red}!%f'
    elif zgit_hasuntracked; then
        output='%F{blue}?%f'
    elif ! zgit_isworktreeclean; then
        output='%F{yellow}#%f'
    elif zgit_hasuncommited; then
        output='%F{white}#%f'
    else
        output='%F{green}#%f'
    fi

    echo -n $output

    return 0
}

git_branchstatus() {
    zgit_isgit || return 1

    local -a ahead behind 

    if zgit_tracking_merge &> /dev/null; then
        echo -n "  %F{green}[git:$(zgit_head)"

        ahead=($(git rev-list --reverse $(zgit_tracking_merge)..HEAD))
        behind=($(git rev-list --reverse HEAD..$(zgit_tracking_merge)))

        if [ $#ahead -gt 0 ]; then
            echo -n " +$#ahead"
        elif [ $#behind -gt 0 ]; then
            echo -n " -$#behind"
        fi
    else
        echo -n "  %F{yellow}[git:$(zgit_head)"
    fi

    echo "]%f"
}

myprompt() {
    git_filestatus || echo ' #'
}

export PS2="> "

if [[ $UID -eq '0' ]]; then;
    export PS1="%F{red}%n%f %1. %(?.%#.%F{red}%#%f) "
    export RPS1="%F{magenta}%T%f"
elif [[ $USER = 'dev' ]]; then;
    export PS1="%F{yellow}%n%f %1.%# "
    export RPS1="%F{magenta}%T%f"
else;
    export PS1="%F{cyan}%n%f %1.%\$(git_branchstatus) \$(myprompt) "
    export RPS1="%F{magenta}%T%f"
fi;

# show the hostname when we are connected through ssh
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    if [[ $UID -eq '0' ]]; then;
        export PS1="%M $PS1"
    elif [[ $USER = 'dev' ]]; then;
        export PS1="%M $PS1"
    else;
        export PS1="%M $PS1"
    fi;
fi; 
#add-zsh-hook precmd gitprompt
