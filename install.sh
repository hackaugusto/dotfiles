#!/usr/bin/bash
# vim: ts=4 sts=4 sw=4 et ft=sh:

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
    msg "Installing packages"
    # adobe-source-sans-pro-fonts
    # ttf-droid
    packages=( \
        ttf-dejavu
        ttf-liberation
        ttf-dejavu
        ttf-fira-mono
        ttf-fira-sans
        cantarell-fonts
        font-mathematica
        xorg-fonts-100dpi
        xorg-fonts-75dpi
        xorg-fonts-alias
        xorg-fonts-encodings
        xorg-fonts-misc
        git
        zsh
        gvim
        sudo
    )

    to_install=()
    for pack in $packages; do
        pacman -Qq $pack > /dev/null 2>&1 || to_install+=("$pack")
    done

    if [ "${#to_install}" -gt 0 ]; then
        sudo pacman -Sy $to_install
    else
        info "All official packages are installed"
    fi

    if ! hash aura > /dev/null 2>&1; then
        require_bin curl
        sudo bash <(curl aur.sh) -S aura-bin
    fi

    if hash aura > /dev/null 2>&1; then
        # ttf-google-fonts-git
        aur_packages=( \
            neovim-git
            python2-neovim-git
        )

        aur_to_install=()
        for aur in $aur_packages; do
            pacman -Qq $aur > /dev/null 2>&1 || aur_to_install+=("$aur")
        done

        if [ "${#aur_to_install}" -gt 0 ]; then
            sudo aura -A $aur_to_install
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

if hash pacman > /dev/null 2>&1; then
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
link .Xdefaults
link .colours

link .bin
link .gitignore_global
link .pdbrc
link .pythonrc
link .xmonad
link .screenrc
link .tmux.conf

link .vim
link .vimrc
link .vim .nvim
link .vimrc .nvimrc

msg 'Vim plugins'
repo 'https://github.com/gmarik/Vundle.vim.git' "${HOME}/.vim/bundle/Vundle.vim"
vim -u ${HOME}/.vim/plugins.vim +PluginUpdate +qa

# TODO: use neobundle or vim-plug
find -L ${HOME}/.vim -iname Makefile | grep -v -e html5.vim -e Dockerfile | while read plugin; do
    info $plugin
    (
        cd $(dirname $plugin);
        make
    ) > /dev/null
done

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
