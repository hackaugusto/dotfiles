bindkey -v

[[ -n "${terminfo[khome]}" ]]  && bindkey "${terminfo[khome]}" beginning-of-line
[[ -n "${terminfo[kend]}"  ]]  && bindkey "${terminfo[kend]}"  end-of-line
[[ -n "${terminfo[kich1]}" ]]  && bindkey "${terminfo[kich1]}" overwrite-mode
[[ -n "${terminfo[kdch1]}" ]]  && bindkey "${terminfo[kdch1]}" delete-char
[[ -n "${terminfo[kpp]}"   ]]  && bindkey "${terminfo[kpp]}"   beginning-of-buffer-or-history     # key previous page
[[ -n "${terminfo[knp]}"   ]]  && bindkey "${terminfo[knp]}"   end-of-buffer-or-history           # key next page

[[ -n "${terminfo[kcuu1]}" ]]  && bindkey "${terminfo[kcuu1]}" history-beginning-search-backward  # up-line-or-history
[[ -n "${terminfo[kcud1]}" ]]  && bindkey "${terminfo[kcud1]}" history-beginning-search-forward   # down-line-or-history
[[ -n "${terminfo[kcub1]}" ]]  && bindkey "${terminfo[kcub1]}" backward-char
[[ -n "${terminfo[kcuf1]}" ]]  && bindkey "${terminfo[kcuf1]}" forward-char

# There is no terminfo capability for <Ctrl> <Right> or <Ctrl> <Left>
# Use (Ctrl + v) to insert the escape sequence
[[ $TERM == screen* ]] && bindkey "[D" backward-word
[[ $TERM == screen* ]] && bindkey "[C" forward-word
[[ $TERM == xterm* ]] && bindkey "Od" backward-word
[[ $TERM == xterm* ]] && bindkey "Oc" forward-word

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        printf '%s' "${terminfo[smkx]}"
    }
    function zle-line-finish () {
        printf '%s' "${terminfo[rmkx]}"
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi
