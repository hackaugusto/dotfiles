# vim:ft=zsh:ts=4:sts=4:sw=4:

export BROWSER='less'
# export BROWSER='most -s'
export ACK_PAGER_COLOR="less -x4SRFX"
export CLASSPATH=~/projects/java:$CLASSPATH
export EDITOR=vim
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export MANSECT=3:1:9:8:2:5:4:7:6:n
export PATH=$HOME/.bin:$PATH
if [ -d ~/.cask ]; then
    export PATH=$HOME/.cask/bin:$PATH
fi
# export PATH=~/.bin:$PATH:~/.android_sdk/tools:~/.android_sdk/platform-tools
export PAGER='less'
# export PAGER='most -s'

#export hlalib="/mnt/files/files/code/assembly/hla/hlalib"
#export hlainc="/mnt/files/files/code/assembly/hla/include"
#export RPHP_IR_PATH=/mnt/ext/var/abs/local/rphp/src/rphp-buil/lib
#export RPHP_RUNTIME_PATH=~/.bin
#export LD_LIBRARY_PATH=~/.lib:$LD_LIBRARY_PATH
#export RPHP_IR_PATH RPHP_RUNTIME_PATH LD_LIBRARY_PATH

# eval `dircolors`

export LESS='-R'
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Do not forget to source /etc/profile !
#
# UTF8 is set system wide with `localectl set-locale en_us.utf8`, this will
# change the contents of /etc/locale.conf to `LANG=en_US.utf8` and
# /etc/profile.d/locale.sh will source it, as of POSIX.1‐2008 [see locale(1)]
# LANG is the default value for the LC_* that are unset, since they are all
# unset all the variables will have the same value
#
# UTF-8 Sexiness!
# export LESSCHARSET="utf-8"
# export LOCALE="en_US.utf8"
# export LANG=en_US.utf8
# export LC="en_US.utf8"
# export LC_CTYPE="en_US.utf8"
# export LC_NUMERIC="en_US.utf8"
# export LC_TIME="en_US.utf8"
# export LC_COLLATE="en_US.utf8"
# export LC_MONETARY="en_US.utf8"
# export LC_MESSAGES="en_US.utf8"
# export LC_PAPER="en_US.utf8"
# export LC_NAME="en_US.utf8"
# export LC_ADDRESS="en_US.utf8"
# export LC_TELEPHONE="en_US.utf8"
# export LC_MEASUREMENT="en_US.utf8"
# export LC_IDENTIFICATION="en_US.utf8"
# export LC_ALL="en_US.utf8"
