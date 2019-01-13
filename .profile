# vim: sts=4 sw=4 et:

# locale (changes /etc/locale.conf)
#   localectl set-locale en_us.utf8

# keymap (changes /etc/X11/xorg.conf.d/00-keyboard.conf)
#   localectl set-x11-keymap br,us abnt2,pc105 ,dvorak terminate:ctrl_alt_bksp,grp:rctrl_toggle,ctrl:nocaps,ctrl:lctrl_meta
#   localectl set-x11-keymap us asus_laptop '' ctrl:swapcaps
# or
#   setxkbmap -layout br,us -model abnt2,pc105 -variant ,dvorak -option terminate:ctrl_alt_bksp,grp:alt_shift_toggle

# Disable Esc as meta in the multiplexers, otherwise evil is unusable
#   screen: maptimeout 5
#   tmux: set -g escape-time 0

# Run gpg-agent through systemd
#   systemctl --user enable gpg-agent-ssh.socket
#   systemctl --user enable dirmngr.socket

# pythonz really don't cut it, I need multiple versions and the ability to
# patch the source before compilation, so I need to use pyenv
#
# - patch before compilation [issue #91] and [https://github.com/yyuu/pyenv/tree/master/plugins/python-build#applying-patches-to-python-before-compiling]
# - multiple versions [issue #167 and #218] and [https://github.com/s1341/pyenv-alias]

# pyenv:
#   Force the use of ucs4 because arch's is compiled with it
#
#   PYTHON_CONFIGURE_OPTS="--enable-shared --enable-unicode=ucs4" pyenv install 2.7.11
#   PYTHON_CONFIGURE_OPTS="--enable-shared --enable-unicode=ucs4" pyenv install 3.5.1
#   VERSION_ALIAS="2.7.11-debug" PYTHON_CONFIGURE_OPTS="--with-pydebug --enable-shared --enable-unicode=ucs4" CC=gcc PYTHON_CFLAGS="-Og -ggdb3" pyenv install 2.7.11
#   VERSION_ALIAS="3.5.1-debug" PYTHON_CONFIGURE_OPTS="--with-pydebug --enable-shared --enable-unicode=ucs4" pyenv install 3.5.1
#
#   git clone https://github.com/haypo/pytracemalloc.git
#   cat pytracemalloc/patches/2.7/pep445.patch | filterdiff --strip=1 | VERSION_ALIAS="2.7.8-trace" PYTHON_CONFIGURE_OPTS="--with-pydebug --enable-shared --enable-unicode=ucs4" pyenv install -p -v 2.7.8

bin() {
    command -v $1 >/dev/null 2>&1
}

running() {
    pgrep -x -u "${USER}" $1 >/dev/null 2>&1
}

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
        # export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
        export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
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
    # don't forget to add the key with ssh-add

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

    # RUST
    [[ -d ~/.cargo ]] && path_addonce_end "$HOME/.cargo/bin"

    # GO
    bin go && {
        [ ! -e ~/.go ] && mkdir -p ~/go/{bin,src}
        [ -d ~/.go ] && export GOPATH="${HOME}/.go"
        [ -d ~/.go ] && path_addonce_end "$GOPATH/bin"
    }

    # OCAML
    load ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

    # NODE
    bin npm && path_addonce_start "$(npm config get prefix)/bin"

    # PYTHON
    load /usr/bin/virtualenvwrapper_lazy.sh

    # https://docs.python.org/2/install/index.html#alternate-installation-the-user-scheme
    USER_BASE=$(python -m site --user-base)
    path_addonce_start "${USER_BASE}/bin"

    # LOCAL BINs
    path_addonce_start "$HOME/.bin"
}

# XDG variables and LANG, LC_* (this sources only *.sh files)
load /etc/profile
load ~/.config/user-dirs.dirs

configure_runtimes

eval $(dircolors)
stty -ixon

export GPG_TTY=$(tty)
export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
export PYTHONSTARTUP=$HOME/.pythonrc

# parallel builds with gnu make
export MAKEFLAGS='-j4'

[ -z "$DISPLAY" -a "$XDG_VTNR" -eq 1 ] && {
    # HiDPI scaling
    export GDK_SCALE=2
    export GDK_DPI_SCALE=0.5

    # remaps Caps to Ctrl (remapping caps with x11 keymap options)
    # [[ ! -z $DISPLAY && -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
    exec startx
}

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty2 ]]; then
    # export XKB_DEFAULT_LAYOUT=us,de
    # export XKB_DEFAULT_VARIANT=",nodeadkeys"
    # export XKB_DEFAULT_OPTIONS="ctrl:swapcaps,"
    export XKB_DEFAULT_LAYOUT=br
    export XKB_DEFAULT_OPTIONS=ctrl:swapcaps
    export XDG_SESSION=wayland
    # exec sway
    # exec dbus-launch --sh-syntax --exit-with-session sway
    # exec gnome-session
fi
