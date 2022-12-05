# Augusto Hack <hack dot augusto at gmail dot com>
# vim:ts=2 sts=2 sw=2:

load(){
    [[ -f $1 ]] && . $1
}

require() {
    [[ -f $1 ]] || echo "Missing file $1"
    [[ -f $1 ]] && . $1
}

bin() {
    command -v $1 >/dev/null 2>&1
}

older_than_days() {
    file=$1
    days=$2

    file_date=$(date -r $file '+%s')
    curr_date=$(date '+%s')

    (( ( ( $curr_date - $file_date ) / 86400 ) > $days ))
}

maybe_git_clone() {
    repo="${1}"
    target="${2}"

    [ ! -e "${target}" ] && {
        mkdir -p "${target}"
        git clone "${repo}" "${target}"
    }
}

profile() {
    # Source the system profile files.
    if (){ setopt localoptions nonomatch nocshnullglob; [ -f /etc/profile.d/*.zsh([1]) ] }
    then
        . /etc/profile.d/*.zsh
    fi

    load $HOME/.profile
}

install() {
    # Install zsh plugins
    maybe_git_clone "https://github.com/tarjoilija/zgen.git" "${HOME}/.zgen"
}

update() {
    # Update the installed plugins
    if older_than_days ~/.zgen/_zgen 50; then
        # update if more than 50 days old
        touch ~/.zgen/_zgen
        (cd ~/.zgen && git pull)
    fi

    if [ -e ~/.zgen/init.zsh ] && older_than_days ~/.zgen/init.zsh 30; then
        # update the plugins every 30 days
        zgen update
    fi
}

profile
install

fpath=($fpath "$HOME/.zsh/func")

zmodload zsh/complist  # complist must be loaded before the compinit call

autoload -U promptinit
autoload -U zgitinit
autoload -U add-zsh-hook
autoload -U compinit

zgitinit

require ~/.zsh/options.sh  # set the options early because the shell behavior change
require ~/.zsh/utils.sh    # load the utils, the following files can use them

require ~/.zsh/alias.sh
require ~/.zsh/arch.sh
require ~/.zsh/bindkey.sh
require ~/.zsh/commands.sh
require ~/.zsh/completion.sh
require ~/.zsh/env.sh
require ~/.zsh/plugins.sh
require ~/.zsh/prompt.sh

update

if [[ "$OSTYPE" = darwin* ]]; then
    export PATH=$(deduplicate_path '/sbin' '/bin' '/usr/bin')
    export BROWSER=/Applications/Firefox.app/Contents/MacOS/firefox
fi

load ~/.opam/opam-init/init.zsh
bin vex && eval "$(vex --shell-config zsh)"
