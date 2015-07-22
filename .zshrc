# Augusto Hack <hack dot augusto at gmail dot com>

function load(){ [[ -f $1 ]] && . $1 }
function require() {
    [[ -f $1 ]] || echo "Missing file $1"
    [[ -f $1 ]] && . $1
}

# require '/etc/profile'        # this is done in /etc/.zprofile
require ~/.zsh/modules.sh
require ~/.zsh/alias.sh
require ~/.zsh/env.sh
require ~/.zsh/options.sh
require ~/.zsh/prompt.sh

require ~/.zsh/zgen.sh
zgen-selfupdate() {
    wget "https://raw.githubusercontent.com/tarjoilija/zgen/master/zgen.zsh" -O ~/.zsh/zgen.sh
}
require ~/.zsh/plugins.sh

load /usr/share/zsh/site-contrib/powerline.zsh
load /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh

# we need to load the plugins before
require ~/.zsh/completion.sh
require ~/.zsh/bindkey.sh

# to run `xargs zsh -i -c "shell_function"` without showing fortune
if [[ -z $_FORTUNE ]]; then
    fortune
    export _FORTUNE=1
fi

# OPAM configuration
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
