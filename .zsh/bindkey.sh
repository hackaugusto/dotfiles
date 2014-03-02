# To insert the sequence with the escape character use ^v (Ctrl + v) followed
# by the keyboard key

# vim keymap
bindkey -v
bindkey "^I" complete-word

if [[ $TERM = 'screen' ]]; then;
    bindkey '[1~' beginning-of-line                 # Home
    bindkey '[2~' overwrite-mode                    # Insert
    bindkey '[3~' delete-char                       # Del
    bindkey '[4~' end-of-line                       # End
    bindkey 'OD'  backward-word                     # CTRL <-
    bindkey 'OC'  forward-word                      # CTRL ->
    bindkey '[Z'  reverse-menu-complete
    bindkey '[A'  history-beginning-search-backward # CTRL up
    bindkey '[B'  history-beginning-search-forward  # CTRL down
elif [[ $TERM = 'rxvt' ]]; then;
    bindkey '[7~' beginning-of-line                 # Home
    bindkey '[2~' overwrite-mode                    # Insert
    bindkey '[3~' delete-char                       # Del
    bindkey '[8~' end-of-line                       # End
    bindkey 'Od'  backward-word                     # CTRL <-
    bindkey 'Oc'  forward-word                      # CTRL ->
elif [[ $TERM = 'linux' ]]; then;
    bindkey '[1~' beginning-of-line                 # Home
    bindkey '[2~' overwrite-mode                    # Insert
    bindkey '[3~' delete-char                       # Del
    bindkey '[4~' end-of-line                       # End
    # bindkey '[D'    backward-word                 # CTRL <-
    # bindkey '[C'    forward-word                  # CTRL ->
elif [[ $TERM = 'xterm' ]]; then;
    bindkey 'OH'  beginning-of-line                 # Home
    bindkey '[2~' overwrite-mode                    # Insert
    bindkey '[3~' delete-char                       # Del
    bindkey 'OF'  end-of-line                       # End
    bindkey '[1;5D'   backward-word                 # CTRL <-
    bindkey '[1;5C'   forward-word                  # CTRL ->
fi
