#!/usr/bin/zsh 

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

[[ -f ~/.Xdefaults ]] && xrdb -merge ~/.Xdefaults
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

# does not work here
# stty stop undef

#exec xmonad
#exec $1
exec openbox-session
