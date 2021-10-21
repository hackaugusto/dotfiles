# vim:ft=zsh:ts=4:sts=4:sw=4:

git_filestatus() {
    zgit_isgit || return 1
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

setup_myprompt(){
    export PS2="> "

    local HOUR="%F{blue}%T%f"
    local JOBS="%1(j,%F{yellow}%j%f ,)"
    export RPS1="${JOBS}${HOUR}"

    if [[ $UID -eq '0' ]]; then;
        export PS1="%F{red}%n%f %1. %(?.%#.%F{red}%#%f) "
    elif [[ $USER = 'dev' ]]; then;
        export PS1="%F{yellow}%n%f %1.%# "
    else;
        export PS1="%F{cyan}%n%f %1.%\$(git_branchstatus) \$(myprompt) "
    fi;

    # show the hostname when we are connected through ssh
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        export PS1="%M $PS1"
    fi;
}

# PURE_PROMPT_SYMBOL='>'
# autoload -U promptinit; promptinit
# prompt pure
setup_myprompt
