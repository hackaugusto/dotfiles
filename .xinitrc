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
numlockx &

#use localectl instead
#setxkbmap -layout br

# does not work here
# stty stop undef

[[ -f ~/.Xdefaults ]] && xrdb -merge ~/.Xdefaults
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap


#exec xmonad
#exec openbox-session
#exec openbox
#exec $1

if [[ $1 == "gnome" ]]; then
    exec gnome-session
elif [[ $1 == "unity" ]]; then
    exec lightdm-session
elif [[ -z $1 ]]; then
    exec openbox-session
else
    exec $1
fi
