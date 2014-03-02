fpath=($fpath $HOME/.zsh/func)
. ~/.zsh/func/git-extras.plugin.zsh

zmodload zsh/complist
autoload -Uz compinit
autoload -U zargs
autoload -U promptinit
autoload -U zgitinit add-zsh-hook

compinit
zgitinit

# incompatibily with unix command
#zmodload -a zsh/stat stat
zmodload -a zsh/zpty zpty
zmodload -ap zsh/mapfile mapfile
