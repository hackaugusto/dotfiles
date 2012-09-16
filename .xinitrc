#!/usr/bin/zsh 

xsetroot -cursor_name left_ptr
xset +fp /usr/share/fonts/local
xset fp rehash

#exec xmonad
#exec openbox-session
#exec openbox
exec $1
