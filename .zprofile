[[ -f ~/.profile ]] && . ~/.profile

if (){ setopt localoptions nonomatch nocshnullglob; [ -f /etc/profile.d/*.zsh([1]) ] }
then
    load /etc/profile.d/*.zsh
fi

# zsh specific
. ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
