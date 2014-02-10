#!/usr/bin/zsh 

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
