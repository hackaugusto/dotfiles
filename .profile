# vim: sts=4 st=4 sw=4 et:
# locale (changes /etc/locale.conf)
# localectl set-locale en_us.utf8

# keymap (changes /etc/X11/xorg.conf.d/00-keyboard.conf)
# localectl set-x11-keymap br,us abnt2,pc105 ,dvorak terminate:ctrl_alt_bksp,grp:rctrl_toggle,ctrl:nocaps,ctrl:lctrl_meta
# or
# setxkbmap -layout br,us -model abnt2,pc105 -variant ,dvorak -option terminate:ctrl_alt_bksp,grp:alt_shift_toggle

bin() { command -v $1 >/dev/null 2>&1 }
running() { pgrep -x -u "${USER}" $1 >/dev/null 2>&1 }
load(){
    local file

    for file in $@; do
        test -r "$file"  && . "$file"
    done
}

haspath() {
    # $1 needs to have regex specila characters escaped, otherwise this function will fail
    ! [[ ! $PATH =~ "^$1:|:$1:|:$1$" ]]
}

path_addonce_start() {
    haspath $1 || export PATH="$1:$PATH"
}

path_addonce_end() {
    haspath $1 || export PATH="$PATH:$1"
}

gpgagent() {
    [ ! -z "${SSH_AUTH_SOCK}" ] && return

    [ -e $HOME/.gnupg/S.gpg-agent.ssh ] && {
        unset SSH_AGENT_PID
        export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
        return
    }

    return 1
}

set_if_exists() {
    variable=$1
    value=$2

    # cant use double indirection because of syntax differences between bash
    # and zsh
    eval "[ -z \"\${$variable}\" -a -x '$value' ] && $variable=$value"
}

# TODO: try keychain and envoy
keyagent() {
    # ssh-keygen -t rsa -b 4096 -C "$(whoami)@$(hostname)-$(date -I)"
    local gnupginf sshsocket
    sshsocket=${XDG_RUNTIME_DIR:-/run}/ssh-agent.socket

    # package seahorse 3.18.0-2 doesnt set SSH_ASKPASS
    set_if_exists SSH_ASKPASS /usr/lib/seahorse/seahorse-ssh-askpass
    set_if_exists SSH_ASKPASS /usr/bin/qt4-ssh-askpass
    set_if_exists SSH_ASKPASS /usr/bin/openssh-askpass
    set_if_exists SSH_ASKPASS /usr/bin/x11-ssh-askpass

    [ ! -z "${SSH_AUTH_SOCK}" ] && return

    running gpg-agent && {
        gpgagent && return
    }

    [ -e $sshsocket ] && running ssh-agent && {
        export SSH_AUTH_SOCK=$sshagent
        return
    }

    # ssh-keygen -H > /dev/null 2>&1
    # [ -e ~/.ssh/known_hosts.old ] && rm ~/.ssh/known_hosts.old

    bin gpg-agent && {
        eval $(gpg-agent --sh --enable-ssh-support --daemon) >/dev/null 2>&1
        gpgagent && return
    }

    bin ssh-agent && {
        eval $(ssh-agent) >/dev/null 2>&1
        return
    }
}

python_pytracemalloc() {
    # Helper to build a patched version of CPython-2.7.8 with pytracemalloc
    # support manager with pythonz
    pythonz_dists=~/.pythonz/dists

    python278="${pythonz_dists}/Python-2.7.8.tgz"
    pytracemalloc12="${pythonz_dists}/pytracemalloc-1.2.tar.gz"
    pythontrace278="${pythonz_dists}/Python-trace-2.7.8.tgz"

    [ ! -d $pythonz_dists ] && mkdir -p $pythonz_dists
    [ ! -f $python278 ] && wget -O $python278 http://www.python.org/ftp/python/2.7.8/Python-2.7.8.tgz
    [ ! -f $pytracemalloc12 ] && wget -O $pytracemalloc12 https://pypi.python.org/packages/source/p/pytracemalloc/pytracemalloc-1.2.tar.gz
    [ ! -f $pythontrace278 ] && {
        TMPDIR=$(mktemp -d)
        # trap "rm -rf ${TMPDIR}" SIGKILL SIGHUP EXIT INT

        tar -xzf $python278 -C $TMPDIR
        tar -xzf $pytracemalloc12 -C $TMPDIR

        (
            cd $TMPDIR/Python-2.7.8
            patch -p1 < ../pytracemalloc-1.2/patches/2.7/pep445.patch
            cd $TMPDIR
            tar czf $pythontrace278 ./Python-2.7.8
        )
    }

    pythonz install --file $pythontrace278 2.7.8-trace

}

configure_runtimes() {
    # CASK
    [[ -d ~/.cask ]] && path_addonce_end "$HOME/.cask/bin"

    # GO
    bin go && {
        [ ! -e ~/.go ] && mkdir -p ~/go/{bin,src}
        [ -d ~/.go ] && export GOPATH="${HOME}/.go"
        [ -d ~/.go ] && path_addonce_end "$GOPATH/bin"
    }

    # NODE
    bin npm && path_addonce_start "$(npm config get prefix)/bin"

    # PYTHON
    export WORKON_HOME=~/work/envs
    export PROJECT_HOME=~/work/projects
    load /usr/bin/virtualenvwrapper_lazy.sh

    # https://docs.python.org/2/install/index.html#alternate-installation-the-user-scheme
    USER_BASE=$(python -m site --user-base)
    path_addonce_start "${USER_BASE}/bin"

    # pythonz really don't cut it, I need multiple versions and the ability to
    # patch the source before compilation, so I need to use pyenv
    #
    # - patch before compilation [issue #91] and [https://github.com/yyuu/pyenv/tree/master/plugins/python-build#applying-patches-to-python-before-compiling]
    # - multiple versions [issue #167 and #218] and [https://github.com/s1341/pyenv-alias]
    [ -d ~/.zgen/yyuu/pyenv-master/ ] && {
        export PYENV_VIRTUALENV_DISABLE_PROMPT=1
        export PYENV_ROOT="$HOME/.pyenv"
        export PATH="$HOME/.zgen/yyuu/pyenv-master/bin:$HOME/.zgen/yyuu/pyenv-master/plugins/python-build/bin:$PATH"
        eval "$(pyenv init -)"
    }

    # LOCAL BINs
    path_addonce_start "$HOME/.bin"
}

load /etc/profile           # XDG variables and LANG, LC_* (this sources only *.sh files)
load $HOME/.nvm/nvm.sh      # This loads NVM
load ~/.config/user-dirs.dirs

configure_runtimes

# ssh-agent should be executed before the x server (to share the agent among all pty)
[ -z "$DISPLAY" ] && bin gpg-agent || keyagent

# otherwise we use the gpg-agent and we need to run it after x server
# (otherwise the dialog box wont work)
[ -n "$DISPLAY" ] && keyagent

eval $(dircolors)
stty -ixon

export GPG_TTY=$(tty)

# [[ ! -z $DISPLAY && -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap      # remaps Caps to Ctrl (remapping caps with x11 keymap options)
[ -z "$DISPLAY" -a "$XDG_VTNR" -eq 1 ] && exec startx
