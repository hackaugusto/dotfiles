[[ -f ~/.profile ]] && . ~/.profile

if (){ setopt localoptions nonomatch nocshnullglob; [ -f /etc/profile.d/*.zsh([1]) ] }
then
    load /etc/profile.d/*.zsh
fi

load ~/.opam/opam-init/init.zsh
bin pip && eval "`pip completion --zsh`"
bin vex && eval "$(vex --shell-config zsh)"
