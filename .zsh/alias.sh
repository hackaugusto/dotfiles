# vim:ft=zsh:ts=4:sts=4:sw=4:

alias l=ls
alias ...='cd ../..'
alias ....='cd ../../..'

alias -s tex=vim
alias -s c=vim
alias -s cpp=vim

alias ag='ag --pager="less -R"'
alias difftree='rsync -crv --dry-run '
# on terminal ESC acts as Meta, horrible to use the evil mode
#alias emacs='emacs -nw'
alias gcc='colorgcc'
alias info='info --vi-keys'
alias vi='vim -p'

#alias chromium='chromium --ignore-gpu-blacklist'
#alias grep='grep --color=auto'
#alias grep='ack --pager="less -R"'
#alias ack='ack --pager="less -R"'

if (( ! $+commands[ack] && $+commands[ack-grep] )); then
    alias ack='ack-grep';
fi

# TODO: [- or -] may happen inside a regex, needs to change the start and end for deletion
if (( $+commands[wdiff] )); then
    filter_changes='/\[-/,/-\]/; /{\+/,/\+}/'

    if (( $+commands[cwdiff] )); then
        function _diff(){ \wdiff $@ | awk $filter_changes | cwdiff -f }
    else
        function _diff(){ \wdiff $@ | awk $filter_changes }
    fi
elif (( $+commands[colordiff] && $+commands[diff] )); then
    function _diff(){ \diff $@ | colordiff }
fi

if whence _diff > /dev/null; then
    alias diff=_diff
fi

if (( $+commands[php] )); then
    function urlencode() { php -r '$s = isset($argv[1]) ? $argv[1] : fgets(STDIN); echo urlencode($s) . "\n";' $@ }
    function urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }
fi

if (( $+commands[python] )); then
    if ! whence _diff > /dev/null; then
        function urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }
    fi
    function format() { python2.7 -c "import sys; print sys.argv[1].format(*(sys.argv[2:] if len(sys.argv) > 2 else sys.stdin))" $@; }
fi

# https://nicholassterling.wordpress.com/2012/03/30/a-zsh-map-function/
function mapa() {
    typeset f="\$[ $1 ]"; shift
    map "$f" "$@"
}

# arch's: /usr/lib/initcpio/functions
map() {
    local r=0
    for _ in "${@:2}"; do
        "$1" "$_" || (( $# > 255 ? r=1 : ++r ))
    done
    return $r
}

function urlencodestream() {
    mapa urlencode
}


function smart_listing(){
    # This is a "smart" ls
    # the only reason for this is because the -B flag is not overwritten by the -a/-A flags
    # this is not gonna list vim backup files on normal ls, but is gonna list them when the all flag is set
    show_all=false
    i=1
    for arg in $@; do
        if [[ $arg == '-B' ]]; then
            argv[$i]=()
        elif [[ $arg == '-a' ]]; then
            show_all=true
            argv[$i]=()
        elif [[ $arg == '-A' ]]; then
            show_all=true
            argv[$i]=()
        fi
        ((i=i+1))
    done

    if ($show_all); then
        show_all='-A'
    else
        show_all='-B'
    fi
    \ls --color=auto $show_all $argv
}
alias ls=smart_listing

# function python_fallback(){
#     if  [[ $# == 0 ]]; then
#         /usr/bin/env python;
#         return;
#     fi;
#     error=$(/usr/bin/env python $@ 2&>1)
#     if [[ $? != 0 ]]; then;
#         /usr/bin/env python2 $@;
#         if [[ $? != 0 ]]; then;
#             echo $error;
#             return $?;
#         fi;
#     fi;
# }
#alias python=python_fallback

# function catwrapper(){
#     prog=$1; shift;
#     pyg_args=$1; shift;
#     $prog $@ | pygmentize $pyg_args;
# }
# alias cat='wrapper cat -g'
