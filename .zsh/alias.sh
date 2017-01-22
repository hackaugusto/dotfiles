# vim:ft=zsh:ts=4:sts=4:sw=4:

alias ...='cd ../..'
alias ....='cd ../../..'

alias ag='ag --pager="less -R"'
alias difftree='rsync -crv --dry-run '
alias emacs='emacs -nw'
alias info='info --vi-keys'
alias vi='vim -p'
alias vim='vim -p'
alias gdb='gdb --silent --nowindows'
alias drop-caches='echo 3 | sudo tee /proc/sys/vm/drop_caches'
