#!/bin/zsh

# cp /etc/X11/xinit/xserverrc ~/.xserverrc

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xsetroot -cursor_name left_ptr
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b
xset m 8/1 0  # mouse acceleration, 8x without threshold
numlockx &

[[ -f $HOME/.Xresources ]] && xrdb -I$HOME ~/.Xresources
[[ -f $HOME/.xbindkeysrc ]] && xbindkeys 2> /dev/null
# [[ -f $HOME/.Xmodmap ]] && xmodmap ~/.Xmodmap         # CTRL changed with x's keyboard options 'ctrl:nocaps,ctrl:lctrl_meta'

if [[ $1 == "gnome" ]]; then
    exec gnome-session
elif [[ $1 == "unity" ]]; then
    exec lightdm-session
elif [[ -z $1 ]]; then
    exec openbox-session
else
    exec $1
fi
