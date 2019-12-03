#!/usr/bin/bash
# vim: ts=4 sts=4 sw=4 et ft=sh:

set -e

error () {
    printf "$(tput bold)$(tput setaf 1) -> $1$(tput sgr0)\n" >&2
}

success () {
    printf "$(tput bold)$(tput setaf 2) -> $1$(tput sgr0)\n"
}

msg() {
    printf "$(tput bold)$(tput setaf 2) $1$(tput sgr0)\n"
}

info() {
    printf "$(tput bold)$(tput setaf 4) -> $1$(tput sgr0)\n"
}

die() {
    error "$1"
    exit 1
}

bin() {
    hash $1 2> /dev/null
}

has_exact_line() {
    file=$1
    line=$2

    egrep "^$line$" $file 2>&1 > /dev/null
}

require_bin() {
    [ $# -ne 1 ] && die "${0} needs 1 argument: ${0} binary"

    bin $1 || {
        die "Required binary was not found ${1}"
    }
}

# TODO: warn if the file exists and it is NOT a link
link() {
    [ $# -lt 1 -o $# -gt 2 ] && die "link needs one or two arguments (got $#): link <file> [destination]"

    file=$1
    target=$HOME/${2:-$1}
    original=$REPO/$1

    [ ! -e "${original}" ] && {
        error "File ${original} does not exists, cannot create link ${target}"
        return
    }

    # remove the current link and proceed
    [ -e "${target}" -a "${FORCE}" -eq 1 ] && {
        unlink "${target}"
    }

    # if the target exist and we don't want to force, print the message and
    # exit, otherwise the ln -s call bellow will fail and the script quit
    [ -e "${target}" ] && {
        info "${target} already exists, skipping"
        return
    }

    [ ! -e $target ] && {
        info "${target} created"
        ln -s "${original}" "${target}"
    }
}

repo() {
    [ $# -ne 2 ] && die "repo needs two arguments (got $#): repo [url] [directory]"

    url=$1
    directory=$2

    [ ! -d $directory ] && \git clone $url $directory
    [ -d $directory ] && (cd $directory && \git pull)
}

is_readable() {
    [ $# -ne 1 ] && die "is_readable needs one arguments (got $#): repo [path]"

    [ -r $1 ] && error "$1 is not readable"
}

arch_pacman() {
    root=''
    [ $UID = 0 ] || root='sudo'

    msg "Installing packages"
    # pacman -Qq won't know that the group was installed
    # gst-plugins-bad
    # gst-plugins-base
    # gst-plugins-good
    # gst-plugins-ugly
    # dnsutils
    # base-devel
    # gstreamer0.10-plugins
    #
    # network managers:
    # wicd / networkmanager
    #
    # network monitor:
    # darkstat
    packages=( \
        pacman-contrib

        # system setup
        grub
        efibootmgr
        nftables

        # window manager
        obconf
        openbox
        sway
        # i3blocks
        i3status

        # printing
        cups
        hplip

        # editing
        gimp

        # display server
        xorg-server
        xorg-xinit

        # desktop applicaitons
        alacritty
        chromium
        firefox-developer-edition
        dmenu
        evince
        feh
        nautilus

        # desktop tools
        arandr
        dialog
        xscreensaver

        # shell tools
        man-db
        pass
        wipe
        scrot
        seahorse
        xclip
        xsel
        bluez
        bluez-utils
        pulseaudio-bluetooth

        # laptop
        acpi
        powertop

        # font
        adobe-source-code-pro-fonts
        adobe-source-han-sans-cn-fonts
        adobe-source-han-sans-jp-fonts
        adobe-source-han-sans-kr-fonts
        adobe-source-han-sans-otc-fonts
        adobe-source-han-sans-tw-fonts
        adobe-source-sans-pro-fonts
        adobe-source-serif-pro-fonts
        cantarell-fonts
        font-mathematica
        terminus-font
        ttf-dejavu
        ttf-dejavu
        ttf-droid
        ttf-fira-mono
        ttf-fira-sans
        ttf-liberation
        xorg-fonts-100dpi
        xorg-fonts-75dpi
        xorg-fonts-alias
        xorg-fonts-encodings
        xorg-fonts-misc

        # la/tex
        # texlive-bin
        # texlive-core
        # texlive-pstricks
        # texlive-latexextra
        # texlive-fontsextra
        # texlive-bibtexextra
        # texlive-pictures
        # texlive-science
        # minted
        # biber

        # multimedia
        alsa-oss
        alsa-tools
        alsa-utils
        flashplugin
        gst-libav
        lib32-alsa-plugins
        mplayer
        pulseaudio
        pulseaudio-alsa
        pavucontrol

        # wine
        # wine
        # winetricks
        # corefonts

        # editor
        emacs
        neovim
        python-neovim
        lua-lpeg

        # terminal/shell
        tmux
        zsh
        openssh

        # shell utils
        aria2
        gnu-netcat
        graphviz
        htop
        iotop
        jq
        lsof
        moreutils
        ncdu
        net-tools
        parallel
        patchutils
        sudo
        sysstat
        the_silver_searcher
        tree
        unzip
        wget
        zip

        # rust
        rustup  # conflicts with rust and cargo
        rustfmt
        rust-racer

        # c/c++
        clang
        clang-analyzer
        clang-tools-extra
        llvm
        gcc-multilib

        # libs
        boost
        lapack

        # build
        cmake
        ninja
        ccache
        automake
        autoconf
        m4

        # python
        pygmentize
        pypy
        pypy3
        python
        python2
        python2-virtualenv
        python-pipenv
        python-virtualenv
        python-black

        # version control
        git
        tig

        # ethereum
        solidity
        geth

        # haskell
        # ghc
        # ghc-static
        # idris
        # darcs
        # hlint
        # $ stack setup
        # $ stack install stylish-haskell hlint hasktags ghc-mod hasktags
        # stack

        # other programming
        shellcheck

        # tools
        # used in ~/.gitconfig
        diff-so-fancy
        dmidecode
        ctags
        dwdiff
        # for ~/.bin/pxml
        python-lxml

        # tracing
        strace
        ltrace

        # profilling/benchmark
        # oprofile
        perf
        siege
        # valgrind
        # kcachegrind

        # virtual machine/containers
        # docker
        # docker-compose
        # containerd
        # runc
        # virtualbox
        # virtualbox-guest-iso
        # virtualbox-host-modules-arch

        # debugging
        lldb
        gdb
        # required by .gdb/c/longlist.py
        python-pygments
        python-pycparser

        # esp32
        picocom
        dtc
        dfu-util
        gperf
        python2-pyserial
        python2-cryptography
        python2-future
        # python2-pyelftools
    )

    # on a fresh install update prior to querying
    $root pacman -Sy

    # install the base groups
    $root pacman -S base base-devel

    to_install=()
    for pack in $packages; do
        pacman -Qq $pack > /dev/null 2>&1 || to_install+=("$pack")
    done

    if [ "${#to_install}" -gt 0 ]; then
        $root pacman -Sy $to_install
    else
        info "All official packages are installed"
    fi
}

arch_aur(){
    # anything bellow needs to run unprivileged, mostly because of makepkg
    [ $UID = 0 ] && return

    is_readable $AURDL

    # ttf-google-fonts-git
    # reflector
    # terraform
    # fzf
    # fzf-extras-git
    # packer-io
    # powerpill
    # neovim-git
    # python2-neovim-git
    # python37

    # Emacs and C/C++
    # Alternaties:
    # - simple parsers: ctags gtags
    # - clang based parsers: rtags irony-mode ccls emacs-ycmd
    #
    # Vim:
    # - Using deoplete with jedi instead of vim-youcompleteme-git
    aur_packages=( \
        aurutils

        otf-hack
        otf-pragmatapro
        ttf-iosevka
        ttf-font-awesome

        bear
        cotire

        ccls

        alacritty-git
        chromium-pepper-flash-dev
        # colout-git - using pygmentize directly
        dropbox

        abntex2
        opam
        flamegraph-git
        notify-osd-customizable
        paperkey
        powerpill
        rust-src
        jdk
        jre
        rr
        secp256k1-git
        wrk
        wrk2-git
        pup-git
        ruby-neovim
        pssh
        gtklp
        # grafana-bin
        # urxvt-resize-font-git
        tiptop
        rust-clippy-git
        tla-tools
        include-what-you-use

        tzupdate
    )

    for package in $aur_packages; do
        ${AURDL} $package
        sudo pacman -Syu $package/*.pkg.*
    done
}

check_configuration() { 
    # Depedencies for compilation
    has_exact_line /etc/pacman.conf "\[multilib\]" || {
        info 'Add the following to /etc/pacman.conf'
        echo
        echo '[multilib]'
        echo 'Include = /etc/pacman.d/mirrorlist'
        echo
        die "multilib has to be enabled"
    }

    echo
    echo
    msg 'Edit the /etc/makepkg.conf file and remove the strip option:'
    echo 'OPTIONS+=(debug !strip)'
    echo
    echo

    # grep -i '^\[xyne-x86_64\]' /etc/pacman.conf || {
    #     error 'Missing configuration section for xyne tools (used for powerpill)'
    #     echo
    #     info 'Add the Xyne repo into the /etc/pacman.conf'
    #     echo
    #     echo '[xyne-x86_64]'
    #     echo 'SigLevel = Required'
    #     echo 'Server = http://xyne.archlinux.ca/repos/xyne'
    #     echo
    #     info 'and import xyne key (https://xyne.archlinux.ca/#signing-key)'
    #     echo
    #     echo 'gpg --recv-keys EC3CBE7F607D11E663149E811D1F0DC78F173680'
    #     echo
    #     echo
    # }

    grep -i '^\[aur\]' /etc/pacman.conf || {
        error 'Missing configuration section for aurtools'
        echo
        info 'Add the following to /etc/pacman.conf'
        echo
        echo '[aur]'
        echo 'SigLevel = Optional TrustAll'
        echo 'Server = file:///var/cache/pacman/aur/'
        echo
        info 'and execute the following commands'
        echo
        echo 'sudo install -d /var/cache/pacman/aur -o $USER'
        echo 'repo-add /var/cache/pacman/aur/aur.db.tar'
        echo
        echo
    }

    # $SUDO grep -i '^pt_br.utf-8' /etc/locale.gen || {
    #     error 'Missing locale pt_br on file /etc/locale.gen'
    #     info 'echo "pt_BR.UTF-8" >> /etc/local.gen'
    # }

    $SUDO grep -i '^en_us.utf-8' /etc/locale.gen || {
        error 'Missing locale en_us on file /etc/locale.gen'
        info 'echo "en_US.UTF-8" >> /etc/local.gen'
    }

    [ ! -e /etc/locale.conf ] && {
        echo
        echo 'Run the following command to set the system locale'
        msg 'localectl set-locale LANG=en_US.UTF-8'
        echo
        echo
        echo 'Run the following command to set X.org keymap'
        msg 'localectl set-x11-keymap br,us abnt2,pc105 ,dvorak terminate:ctrl_alt_bksp,grp:rctrl_toggle,ctrl:nocaps,ctrl:lctrl_meta'
        echo or
        msg 'setxkbmap -layout br,us -model abnt2,pc105 -variant ,dvorak -option terminate:ctrl_alt_bksp,grp:alt_shift_toggle'
        echo
        echo
    }

    # is_readable /etc/hostname
    # [ ! -e /etc/hostname ] && {
    #     erro 'Missing /etc/hostname file'
    #     info 'hostnamectl set-hostname <hostname>'
    #     echo
    #     info 'And add the hostname into the /etc/hosts file'
    #     echo
    #     echo
    # }

    [ ! -e /etc/localtime ] && {
        error 'Missing /etc/localtime file'
        info 'ln -s /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime'
    }

    [ ! -e /etc/sysctl.d/50-dmesg-restrict.conf ] && {
        error 'Missing /etc/sysctl.d/50-dmesg-restrict.conf file'
        info 'echo "kernel.dmesg_restrict = 1" > /etc/sysctl.d/50-dmesg-restrict.conf'
    }

    [ ! -e /etc/sysctl.d/50-kptr-restrict.conf ] && {
        error 'Missing /etc/sysctl.d/50-kptr-restrict.conf file'
        info 'echo "kernel.kptr_restrict = 2" > /etc/sysctl.d/50-kptr-restrict.conf'
        info 'this break perf'
    }

    groups hack | grep uucp 2>&1 > /dev/null || {
        info 'The user hack does not have the group uucp'
        info 'using /dev/ttyUSB0 will not work'
        info 'To fix, use:'
        info '   usermod -a -G uucp '
    }

    is_readable /etc/locale.conf
    is_readable /var
    is_readable /var/lib
    is_readable /var/lib/pacman/local/ALPM_DB_VERSION
    is_readable /var/log
}

# check_configuration

[ $SHELL != "/bin/zsh" ] && die "This script does not work with ${SHELL}, only works with zsh"

FORCE=0
while getopts "f" opt; do
    case $opt in
        f)
            FORCE=1
            ;;
    esac
done
shift $(($OPTIND-1))

require_bin git
require_bin nvim
require_bin pacman

REPO=${HOME}/.dotfiles
AURDL=${REPO}/.bin/aurdl

repo 'https://github.com/hackaugusto/dotfiles.git' "$REPO"

link .bash_profile
link .bashrc
link .profile
link .zshrc
link .zsh
link .makepkg.conf

mkdir -p $HOME/.gnupg
link .gnupg/gpg.conf
link .gnupg/gpg-agent.conf

link .xinitrc
link .xbindkeysrc
link .XCompose
link .Xresources
link .Xresources.d

# mkdir -p $HOME/.config/fontconfig/conf.d
# link /etc/fonts/conf.avail/10-sub-pixel-rgb.conf .config/fontconfig/conf.d/10-sub-pixel-rgb.conf

link .bin
link .pdbrc
link .xmonad
link .screenrc
link .tmux.conf
link .urxvt
link .urxvt/resize-font
link .latexmkrc
link .sqliterc
link .gitconfig

mkdir -p $HOME/.gdb/{c,cpp,py}
link .gdbinit
link .gdb/config
link .gdb/c/locallist
link .gdb/c/color
link .gdb/c/longlist.py
link .gdb/cpp/__init__.py
link .gdb/cpp/printers.py
link .gdb/cpp/xmethods.py
link .gdb/py/libpython.py

# git config --global core.excludesfile '~/.gitignore_global'
link .gitignore_global

mkdir -p ${HOME}/.emacs.d/lisp
link .emacs.d/init.el
# repo https://github.com/ProofGeneral/PG ~/.emacs.d/lisp/PG
# (cd ~/.emacs.d/lisp/PG && make)

mkdir -p ${HOME}/.config
link .config/flake8
link .config/pep8
link .config/pylintrc
link .config/user-dirs.dirs

mkdir -p ${HOME}/.config/alacritty/
link .config/alacritty/alacritty.yml

mkdir -p ${HOME}/.config/openbox
link .config/openbox/autostart.sh
link .config/openbox/multimonitor.xml
link .config/openbox/rc.xml

mkdir -p ${HOME}/.config/nvim/plugins/repos/github.com/Shougo
link .config/nvim/init.vim

arch_pacman

arch_aur

SUDO=''
[ $UID = 0 ] || SUDO='sudo'
