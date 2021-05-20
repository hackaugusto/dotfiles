# vim:ft=zsh:ts=4:sts=4:sw=4:

GIT_ASYNC_WORKER=zgit_worker

# Formats the prompt based on VCS state
myprompt_print_vcs_details() {
    local vcs_state vcs_head
    local -a ahead behind

    if ! zgit_isgit; then
        vcs_state='%#'
        vcs_head=''
    elif ! zgit_inworktree; then
        vcs_state='%#'
        vcs_head=''
    else
       if zgit_hasunmerged; then
            vcs_state='%F{red}!%f'
        elif zgit_hasuntracked; then
            vcs_state='%F{blue}?%f'
        elif ! zgit_isworktreeclean; then
            vcs_state='%F{yellow}%#%f'
        elif zgit_hasuncommited; then
            vcs_state='%F{white}%#%f'
        else
            vcs_state='%F{green}%#%f'
       fi

        if zgit_tracking_merge &> /dev/null; then
            vcs_head="%F{green}[git:$(zgit_head)]%f "

            ahead=($(git rev-list --reverse $(zgit_tracking_merge)..HEAD))
            behind=($(git rev-list --reverse HEAD..$(zgit_tracking_merge)))

            if [ $#ahead -gt 0 ]; then
                echo -n " +$#ahead"
            elif [ $#behind -gt 0 ]; then
                echo -n " -$#behind"
            fi
        else
            vcs_head="%F{yellow}[git:$(zgit_head)]%f "
        fi
    fi

    print " ${vcs_head}${vcs_state}"
}

myprompt_job() {
    cd -q $1
    zgit_info_update
    myprompt_print_vcs_details
}

# executed when a job is finished
myprompt_callback_update_prompt() {
    typeset -g myprompt
    myprompt=$3
    zle reset-prompt
}

# executed before every prompt, starts the background job
myprompt_callback_start_background_job() {
    async_flush_jobs ${GIT_ASYNC_WORKER}
    async_job ${GIT_ASYNC_WORKER} myprompt_job ${PWD}
}

myprompt_setup_async_worker() {
    async_init
    async_start_worker ${GIT_ASYNC_WORKER}
    async_register_callback ${GIT_ASYNC_WORKER} myprompt_callback_update_prompt
    add-zsh-hook precmd myprompt_callback_start_background_job
}

# One time setup for all prompts.
#
# Users root and dev use simpler prompts.
# "Normal" users will use an asynchronous worker to fetch VCS data.
#
myprompt_setup(){
    typeset -g myprompt

    # First setup prompts that don't need a worker
    export PS2="> "

    local HOUR="%F{blue}%T%f"
    local JOBS="%1(j,%F{yellow}%j%f ,)"
    export RPS1="${JOBS}${HOUR}"

    if [[ $UID -eq 0 ]]; then
        export PS1="%F{red}%n%f %1. %(?.%#.%F{red}%#%f) "
    elif [[ $USER = 'dev' ]]; then
        export PS1="%F{yellow}%n%f %1.% %# "
    else
        myprompt_setup_async_worker
        export PS1="%F{cyan}%n%f %1.% \${myprompt} "
    fi;

    # Show the hostname when we are connected through ssh
    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        export PS1="%M $PS1"
    fi;
}

myprompt_setup
