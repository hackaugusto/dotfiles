[[ $- != *i* ]] && return       # If not running interactively, don't do anything

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
