# vim:ft=zsh:ts=4:sts=4:sw=4:

autoload up-line-or-beginning-search
autoload down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey -v

[[ -n ${terminfo[khome]} ]]  && bindkey ${terminfo[khome]} beginning-of-line
[[ -n ${terminfo[kend]}  ]]  && bindkey ${terminfo[kend]}  end-of-line
[[ -n ${terminfo[kich1]} ]]  && bindkey ${terminfo[kich1]} overwrite-mode
[[ -n ${terminfo[kdch1]} ]]  && bindkey ${terminfo[kdch1]} delete-char
[[ -n ${terminfo[kpp]}   ]]  && bindkey ${terminfo[kpp]}   beginning-of-buffer-or-history     # key previous page
[[ -n ${terminfo[knp]}   ]]  && bindkey ${terminfo[knp]}   end-of-buffer-or-history           # key next page

[[ -n ${terminfo[kcuu1]} ]]  && bindkey ${terminfo[kcuu1]} up-line-or-beginning-search
[[ -n ${terminfo[kcud1]} ]]  && bindkey ${terminfo[kcud1]} down-line-or-beginning-search
[[ -n ${terminfo[kcub1]} ]]  && bindkey ${terminfo[kcub1]} backward-char
[[ -n ${terminfo[kcuf1]} ]]  && bindkey ${terminfo[kcuf1]} forward-char

# There is no terminfo capability for <Ctrl> <Right> or <Ctrl> <Left>
# Use (Ctrl + v) to insert the escape sequence
[[ $TERM == screen* ]] && bindkey "[D" backward-word
[[ $TERM == screen* ]] && bindkey "[C" forward-word
[[ $TERM == xterm* ]] && bindkey "Od" backward-word
[[ $TERM == xterm* ]] && bindkey "Oc" forward-word
