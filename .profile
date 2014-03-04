# locale (changes /etc/locale.conf)
# localectl set-locale en_us.utf8

# keymap (changes /etc/X11/xorg.conf.d/00-keyboard.conf)
# localectl set-x11-keymap br,us abnt2,pc105 ,dvorak terminate:ctrl_alt_bksp,grp:alt_shift_toggle,ctrl:nocaps,ctrl:lctrl_meta
# or
# setxkbmap -layout br,us -model abnt2,pc105 -variant ,dvorak -option terminate:ctrl_alt_bksp,grp:alt_shift_toggle

[[ -f /etc/profile ]] && . /etc/profile                         # XDG variables and LANG, LC_*
[[ -f $HOME/.nvm/nvm.sh ]] && . $HOME/.nvm/nvm.sh               # This loads NVM

# TODO: find a way add the key on first use without manually calling ssh-add
# Execute the ssh agent before running X so that all GUI share the same session
command -v ssh-agent >/dev/null 2>&1 && [[ -z $SSH_AGENT_PID ]] && eval $(ssh-agent) >/dev/null 2>&1
eval $(dircolors)
stty -ixon

# [[ ! -z $DISPLAY && -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap      # remaps Caps to Ctrl (remapping caps with x11 keymap options)
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
