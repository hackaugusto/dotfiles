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
link .gitignore_global
link .pdbrc
link .profile
link .pythonrc
link .screenrc
link .tmux.conf
link .vimrc
link .xbindkeysrc
link .XCompose
link .xinitrc
link .xmonad
link .zprofile
link .zshrc

link .vim
repo 'https://github.com/gmarik/Vundle.vim.git' "${HOME}/.vim/bundle/Vundle.vim"
vim -u ${HOME}/.vim/plugins.vim +PluginUpdate +qa

repo 'https://github.com/cask/cask.git' "${HOME}/.cask"
link .emacs.d
(cd ${HOME}/.emacs.d/; ${HOME}/.cask/bin/cask install)

link .zsh
link .bin
link .colours

mkdir -p ${HOME}/.config
link .config/flake8
link .config/user-dirs.dirs

mkdir -p ${HOME}/.config/openbox
link .config/openbox/autostart.sh
link .config/openbox/multimonitor.xml
link .config/openbox/rc.xml
