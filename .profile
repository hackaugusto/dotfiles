# vim: sts=4 st=4 sw=4 et:
# locale (changes /etc/locale.conf)
# localectl set-locale en_us.utf8

# keymap (changes /etc/X11/xorg.conf.d/00-keyboard.conf)
# localectl set-x11-keymap br,us abnt2,pc105 ,dvorak terminate:ctrl_alt_bksp,grp:rctrl_toggle,ctrl:nocaps,ctrl:lctrl_meta
# or
# setxkbmap -layout br,us -model abnt2,pc105 -variant ,dvorak -option terminate:ctrl_alt_bksp,grp:alt_shift_toggle

bin() {
    command -v $1 >/dev/null 2>&1
}

running() {
    pgrep -x -u "${USER}" $1 >/dev/null 2>&1
}

gpgagent() {
    [ -z "${GPG_TTY}" ] && {
        GPG_TTY=$(tty)
        export GPG_TTY
    }

    [ ! -z "${SSH_AUTH_SOCK}" ] && return

    [ -e $HOME/.gnupg/S.gpg-agent.ssh ] && {
        unset SSH_AGENT_PID
        export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
        return
    }

    return 1
}

# TODO: try keychain and envoy
keyagent() {
    # SSH_ASKPASS -> seahorse-ssh-askpass, openssh-askpass, x11-ssh-askpass
    # ssh-keygen -t rsa -b 4096 -C "$(whoami)@$(hostname)-$(date -I)"
    local gnupginf sshsocket
    sshsocket=${XDG_RUNTIME_DIR:-/run}/ssh-agent.socket

    [ ! -z "${SSH_AUTH_SOCK}" ] && return

    running gpg-agent && {
        gpgagent && return
    }

    [ -e $sshsocket ] && running ssh-agent && {
        export SSH_AUTH_SOCK=$sshagent
        return
    }

    bin gpg-agent && {
        eval $(gpg-agent --sh --enable-ssh-support --daemon) >/dev/null 2>&1
        gpgagent && return
    }

    bin ssh-agent && {
        eval $(ssh-agent) >/dev/null 2>&1
        return
    }
}

[ -f /etc/profile ] && . /etc/profile                                         # XDG variables and LANG, LC_*
[ -f $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh                               # This loads NVM
hash npm 2>&1 && export PATH="$(npm config get prefix)/bin:$PATH"

# Execute the agent before running X so that all GUI share the same session
keyagent

eval $(dircolors)
stty -ixon

# [[ ! -z $DISPLAY && -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap      # remaps Caps to Ctrl (remapping caps with x11 keymap options)
[ -z "$DISPLAY" -a "$XDG_VTNR" -eq 1 ] && exec startx
