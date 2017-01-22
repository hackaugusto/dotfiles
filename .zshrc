# Augusto Hack <hack dot augusto at gmail dot com>

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

zgen-selfupdate() {
    wget "https://raw.githubusercontent.com/tarjoilija/zgen/master/zgen.zsh" -O "${HOME}/.zsh/func/zgen"
}

profile() {
    # load system wide configuration
    if (){ setopt localoptions nonomatch nocshnullglob; [ -f /etc/profile.d/*.zsh([1]) ] }
    then
        . /etc/profile.d/*.zsh
    fi

    # load user configuration
     load $HOME/.profile
}

older_than_days() {
    file=$1
    days=$2

    file_date=$(date -r $file '+%s')
    curr_date=$(date '+%s')

    (( ( ( $curr_date - $file_date ) / 86400 ) > $days ))
}

install() {
    if [ ! -e ~/.zsh/func/zgen ]; then
        # install zgen if not installed
        zgen-selfupdate
    fi

    if [ ! -e ~/.pyenv/plugins/pyenv-alias ]; then
        git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
        git clone https://github.com/s1341/pyenv-alias.git ~/.pyenv/plugins/pyenv-alias
    fi
}

update() {
    if older_than_days ~/.zsh/func/zgen 50; then
        # update if more than 50 days old
        zgen selfupdate
    fi

    if [ -e ~/.zgen/init.zsh ] && older_than_days ~/.zgen/init.zsh 30; then
        # update the plugins every 30 days
        zgen update

        (
            cd ~/.pyenv/plugins/pyenv-alias
            git pull
        )

        (
            cd ~/.pyenv/plugins/pyenv-virtualenv
            git pull
        )
    fi
}

profile
install

fpath=($fpath $HOME/.zsh/func)

zmodload zsh/complist  # complist must be loaded before the compinit call

autoload -U promptinit
autoload -U zgitinit
autoload -U add-zsh-hook
autoload -U compinit

# load now but does not execute
autoload +X zgen

zgitinit

if [ -s ~/.zcompdump -a ! -s ~/.zcompdump.zwc ]; then
  zcompile ~/.zcompdump &!
fi

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

# compinit must be executed after the plugins are set
# if [ ! -e ~/.zcompdump ] || older_than_days ~/.zcompdump 1; then
#   compinit     # this may or may not recreate the dump
# else
#   # If the dump is less than a day old dont bother recreating it.
#   # !IMPORTANT! this will skip the checks for group/world writable files.
#   compinit -C
# fi

update

# environment
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export PYENV_ROOT="$HOME/.pyenv"

if [[ "$OSTYPE" = darwin* ]]; then
    export PATH=$(deduplicate_path '/sbin' '/bin' '/usr/bin')
fi

load ~/.opam/opam-init/init.zsh
bin vex && eval "$(vex --shell-config zsh)"

# npm completion is slow: "0.37s user"
# if (( $+commands[npm] )); then
#     eval "$(npm completion 2>/dev/null)"
# fi
