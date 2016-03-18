[[ -f ~/.profile ]] && . ~/.profile

if (){ setopt localoptions nonomatch nocshnullglob; [ -f /etc/profile.d/*.zsh([1]) ] }
then
    load /etc/profile.d/*.zsh
fi

load ~/.opam/opam-init/init.zsh
load /etc/profile.d/fzf.zsh
bin vex && eval "$(vex --shell-config zsh)"

bin pip && {
    # pip could be installed on the system OR it can be a pyenv shim, if it is
    # a shim we need to ensure that at least one venv is selected
    script=$(pip completion --zsh 2> /dev/null)

    # pyenv's shim returns 127 on failure
    [ -n "${PYENV_ROOT}" -a $? -eq 127 ] && {
        version=$(pyenv versions | grep -v '^\*' | awk '{print $1}' | head -n1)

        # try again in a subshell, if it fails ... ignore it
        (
            pyenv shell $version;
            script=$(pip completion --zsh 2> /dev/null)
        )
    }

    [ -n "${script}" -a $? -eq 0 ] && eval "${script}"
}
