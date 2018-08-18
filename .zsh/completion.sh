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

# npm completion is slow: "0.37s user"
# if (( $+commands[npm] )); then
#     eval "$(npm completion 2>/dev/null)"
# fi

if (( $+commands[pipenv] )); then
    eval "$(pipenv --completion)"
fi
