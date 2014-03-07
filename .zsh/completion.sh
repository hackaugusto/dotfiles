# vim:ft=zsh:ts=4:sts=4:sw=4:

# does not have a easy way to discard the prediction, using
# tarruda/zsh-autosuggestions instead
# autoload predict-on
# predict-on

# the widget complete-word must be used to do the completion instead of expand-complete,
# otherwise the _expand completer will not work
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format '%d:'
zstyle ':completion:*' group-name ''
# DO NOT forget to eval(`dircolors`)
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
#zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*'
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=3 yes
zstyle ':completion:*:functions' ignored-patterns '_*'

zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' prompt 'Alternatives %e:'
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%B no matches for: %d%b'

zstyle :compinstall filename '/home/hack/.zshrc'

function _pip_completion {
    local words cword
    read -Ac words
    read -cn cword
    reply=($(COMP_WORDS="$words[*]" COMP_CWORD=$(( cword-1 )) PIP_AUTO_COMPLETE=1 $words[1]))
}
compctl -K _pip_completion pip2

function zle-line-init () {
    # make sure the terminal is in application mode, when zle is
    # active. Only then are the values from $terminfo valid.
    if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
        printf '%s' "${terminfo[smkx]}"
    fi
    zle -l | grep autosuggest-start >& /dev/null && zle autosuggest-start
}
function zle-line-finish () {
    if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
        printf '%s' "${terminfo[rmkx]}"
    fi
}
zle -N zle-line-init
zle -N zle-line-finish
