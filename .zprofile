[[ -f ~/.profile ]] && . ~/.profile
load /etc/profile.d/*.zsh   # XDG variables and LANG, LC_* (this sources only *.sh files)

# zsh specific
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
