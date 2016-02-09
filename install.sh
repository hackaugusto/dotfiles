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

require_bin() {
    [ $# -ne 1 ] && die "${0} needs 1 argument: ${0} binary"

    bin $1 || {
        die "Required binary was not found ${1}"
    }
}

link() {
    [ $# -lt 1 -o $# -gt 2 ] && die "link needs one or two arguments (got $#): link <file> [destination]"

    file=$1
    target=$HOME/${2:-$1}
    original=$REPO/$1

    [ ! -e "$original" ] && {
        error "File ${original} does not exists, cannot create link ${target}"
        return
    }
    [ -e "$target" -a "$FORCE" -eq 1 ] && unlink $target
    [ -e $target ] && info "${target} already exists, skipping"
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

archlinux() {
    root=''
    [ $UID = 0 ] || root='sudo'

    msg "Installing packages"
    # adobe-source-sans-pro-fonts
    # ttf-droid
    # using systemd-timesyncd instead of openntpd: timedatectl set-ntp true
    # gccfortran, lapack -> scipy
    #
    # docker:
    #   systemctl enable docker.socket
    #   gpasswd -a <user>  docker
    packages=( \
        cantarell-fonts
        font-mathematica
        terminus-font
        ttf-dejavu
        ttf-dejavu
        ttf-fira-mono
        ttf-fira-sans
        ttf-liberation
        xorg-fonts-100dpi
        xorg-fonts-75dpi
        xorg-fonts-alias
        xorg-fonts-encodings
        xorg-fonts-misc

        chromium
        evince
        firefox
        numlockx
        obconf
        openbox
        texlive-most
        xclip
        xorg
        xorg-xinit

        alsa-oss
        alsa-tools
        alsa-utils
        flashplugin
        gecko-mediaplayer
        gst-libav
        gst-plugins-bad
        gst-plugins-base
        gst-plugins-good
        gst-plugins-ugly
        gstreamer0.10-plugins
        lib32-flashplugin
        mplayer
        pulseaudio
        pulseaudio-alsa
        youtube-dl
        youtube-viewer

        emacs
        fortune-mod
        gvim
        htop
        mosh
        ncdu
        rxvt-unicode
        scrot
        sudo
        the_silver_searcher
        tmux
        tree
        unzip
        wget
        zip
        zsh
        moreutils
        pigz

        base-devel
        boost
        bsdiff
        cargo
        clang
        clang-analyzer
        clang-tools-extra
        colordiff
        ctags
        dnsutils
        docker
        docker-compose
        dwdiff
        fossil
        ftjam
        gcc-fortran
        gdb
        git
        graphviz
        lapack
        ltrace
        net-tools
        openssh
        parallel
        perf
        pssh
        pypy
        pypy3
        python
        python2
        python2-virtualenv
        python-virtualenv
        python-virtualenvwrapper
        re2c
        re2
        rust
        seahorse
        strace
        sysstat
        tig
        valgrind
        virtualbox
        virtualbox-guest-iso
        uwsgi
        uwsgi-plugin-python2
        uwsgi-plugin-python
        uwsgi-plugin-pypy
    )

    to_install=()
    for pack in $packages; do
        pacman -Qq $pack > /dev/null 2>&1 || to_install+=("$pack")
    done

    if [ "${#to_install}" -gt 0 ]; then
        $root pacman -Sy $to_install
    else
        info "All official packages are installed"
    fi
    
    # anything bellow needs to run unprivileged, mostly because of makepkg
    [ $UID = 0 ] && return

    if ! bin aura; then
        require_bin curl
        $root bash <(curl aur.sh) -S aura-bin
    fi

    if bin aura; then
        # ttf-google-fonts-git
        aur_packages=( \
            powerpill
            reflector
            neovim-git
            python2-neovim-git
            notify-osd-customizable
            packer-io
            terraform
            fzf
            fzf-extras-git
            rust-src
            rust-racer
            bear
            wrk
            wrk2-git
        )

        aur_to_install=()
        for aur in $aur_packages; do
            pacman -Qq $aur > /dev/null 2>&1 || aur_to_install+=("$aur")
        done

        if [ "${#aur_to_install}" -gt 0 ]; then
            $root aura -A $aur_to_install
        else
            info "All aur packages are installed"
        fi
    fi
}

FORCE=0
while getopts "f" opt; do
    case $opt in
        f)
            FORCE=1
            ;;
    esac
done
shift $(($OPTIND-1))

if bin pacman; then
    archlinux
fi

require_bin git
require_bin vim

REPO=${HOME}/.dotfiles

repo 'https://github.com/hackaugusto/dotfiles.git' "$REPO"

link .bash_profile
link .bashrc
link .profile
link .zprofile
link .zshrc
link .zsh

link .xinitrc
link .xbindkeysrc
link .XCompose
link .Xresources
link .Xresources.d

# mkdir -p $HOME/.config/fontconfig/conf.d
# link /etc/fonts/conf.avail/10-sub-pixel-rgb.conf .config/fontconfig/conf.d/10-sub-pixel-rgb.conf

link .bin
link .gitignore_global
link .pdbrc
link .gdbinit
link .pythonrc
link .xmonad
link .screenrc
link .tmux.conf

link .vim
link .vimrc
link .vim .nvim
link .vimrc .nvimrc

msg 'Vim plugins'
repo 'https://github.com/hackaugusto/Vundle.vim.git' "${HOME}/.vim/bundle/Vundle.vim"
vim -u ${HOME}/.vim/plugins.vim +PluginUpdate +qa

# TODO: use neobundle or vim-plug
find -L ${HOME}/.vim -iname Makefile | grep -v -e html5.vim -e Dockerfile -e color_coded | while read plugin; do
    info $plugin
    (
        cd $(dirname $plugin);
        make
    ) > /dev/null
done

info 'color_coded'
(
    cd ~/.vim/bundle/color_coded
    mkdir build
    cd build
    cmake ..
    make
    make install
    make clean
    make clean_clang
)

mkdir -p ${HOME}/.emacs.d
link .emacs.d/init.el
link .emacs.d/Cask

repo 'https://github.com/cask/cask.git' "${HOME}/.cask"
(cd ${HOME}/.emacs.d/; ${HOME}/.cask/bin/cask install)

mkdir -p ${HOME}/.config
link .config/flake8
link .config/user-dirs.dirs

mkdir -p ${HOME}/.config/openbox
link .config/openbox/autostart.sh
link .config/openbox/multimonitor.xml
link .config/openbox/rc.xml

if bin pacman; then
    echo
    echo
    msg 'Edit the /etc/makepkg.conf file and remove the strip option:'
    echo 'OPTIONS+=(debug !strip)'
    echo
    echo
    msg 'Add the Xyne repo into the /etc/pacman.conf'
    echo
    echo '[xyne-x86_64]'
    echo 'SigLevel = Required'
    echo 'Server = http://xyne.archlinux.ca/repos/xyne'
    echo
    echo
fi

grep -i '^pt_br.utf-?8' || {
    error 'Missing locale pt_br on file /etc/locale.gen'
    info 'echo "pt_BR.UTF-8" >> /etc/local.gen'
}

grep -i '^en_us.utf-?8' || {
    error 'Missing locale en_us on file /etc/locale.gen'
    info 'echo "en_US.UTF-8" >> /etc/local.gen'
}

[ ! -e /etc/localtime ] || {
    erro 'Missing /etc/localtime file'
    info 'ln -s /usr/share/zoneinfo/America/Sao_Paulo /etc/localtime'
}
