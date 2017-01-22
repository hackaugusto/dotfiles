# vim:ft=zsh:ts=4:sts=4:sw=4:

function deduplicate_path() {
    # split the path, remove duplicates and re-add at the end
    IFS=':'
    duplicates=$@
    echo "${${=PATH}:|duplicates}:${duplicates}"
}

export PAGER='less'
export BROWSER='less'
export EDITOR=vim

export LESS='-R'
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

export PASSWORD_STORE_DIR="${HOME}/.data/password"

export ACK_PAGER_COLOR="less -x4SRFX"
export MANSECT=3:1:9:8:2:5:4:7:6:n

export CLASSPATH=~/projects/java:$CLASSPATH
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
