#!/usr/bin/zsh 

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
xrdb -merge ~/.Xdefaults

# does not work here
# stty stop undef

#exec xmonad
#exec openbox-session
exec openbox
#exec $1
