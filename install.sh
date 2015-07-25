#!/usr/bin/bash
# vim: ts=4 sts=4 sw=4 et:

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

require_bin() {
    [ $# -ne 1 ] && die "${0} needs 1 argument: ${0} binary"

    hash $1 2> /dev/null || {
        die "Required binary was not found ${1}"
    }
}

link() {
    [ $# -ne 1 ] && die "link needs one argument (got $#): link [file]"

    file=$1
    target=$HOME/$file
    original=$REPO/$file

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
repo 'https://github.com/gmarik/Vundle.vim.git' "${HOME}/.vim/bundle/Vundle.vim"
vim -u ${HOME}/.vim/plugins.vim +PluginUpdate +qa

msg 'Building vim plugins'
find -L ${HOME}/.vim -iname Makefile | grep -v html5.vim | while read plugin; do
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

if hash pacman > /dev/null 2>&1; then
    # adobe-source-sans-pro-fonts ttf-droid
    pacman -S ttf-dejavu \
        ttf-liberation\
        cantarell-fonts\
        font-mathematica\
        xorg-fonts-100dpi\
        xorg-fonts-75dpi\
        xorg-fonts-alias\
        xorg-fonts-encodings\
        xorg-fonts-misc

    bash <(curl aur.sh) -S ttf-google-fonts-git
fi
