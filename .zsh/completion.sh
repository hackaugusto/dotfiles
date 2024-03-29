# vim:ft=zsh:ts=4:sts=4:sw=4:

zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=3 yes
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%B no matches for: %d%b'

zstyle :compinstall filename '/home/hack/.zshrc'

zle-line-init() {
    # make sure the terminal is in application mode, when zle is
    # active. Only then are the values from $terminfo valid.
    if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
        printf '%s' "${terminfo[smkx]}"
    fi
}
zle-line-finish() {
    if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
        printf '%s' "${terminfo[rmkx]}"
    fi
}
zle -N zle-line-init
zle -N zle-line-finish

# must be done before compinit
if (( $+commands[rustup] )); then
    if [ ! -f "$HOME/.zsh/func/_rustup" ] || older_than_days "$HOME/.zsh/func/_rustup" 50; then
        rustup completions zsh > "$HOME/.zsh/func/_rustup"
        # force regenerating the cache
        rm ~/.zcompdump
    fi

    CARGO_BIN=$(rustup which cargo) # ${HOME}/.rustup/toolchains/{default}/bin/cargo
    CARGO_DIR=${CARGO_BIN%/*/*} # remove bin/cargo
    CARGO_FUNC="${CARGO_DIR}/share/zsh/site-functions"
    fpath=($fpath "${CARGO_FUNC}")
fi

# compinit must be executed after the plugins are set
if [ ! -e ~/.zcompdump ] || older_than_days ~/.zcompdump 1; then
  compinit     # this may or may not recreate the dump
else
  # If the dump is less than a day old dont bother recreating it.
  # !IMPORTANT! this will skip the checks for group/world writable files.
  compinit -C
fi

# must be done after compinit
if (( $+commands[pipenv] )); then
    eval "$(pipenv --completion)"
fi

# npm completion is slow: "0.37s user"
# if (( $+commands[npm] )); then
#     eval "$(npm completion 2>/dev/null)"
# fi

if [ -s ~/.zcompdump -a ! -s ~/.zcompdump.zwc ]; then
  zcompile ~/.zcompdump &!
fi
