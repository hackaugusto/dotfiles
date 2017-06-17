bin() {
    command -v $1 >/dev/null 2>&1
}

LS_BIN=$(bin ls)

# Make -B overwritable by -a/-A
ls(){
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

    $LS_BIN --classify --color=auto $show_all $argv
}

if (( ! $+commands[ack] && $+commands[ack-grep] )); then
    alias ack='ack-grep';
fi

if (( $+commands[wdiff] )); then
    if (( $+commands[cwdiff] )); then
        diff(){
            /usr/bin/wdiff $@ | awk '/\[-/,/-\]/; /{\+/,/\+}/' | cwdiff -f
        }
    else
        diff(){
            /usr/bin/wdiff $@ | awk '/\[-/,/-\]/; /{\+/,/\+}/'
        }
    fi
elif (( $+commands[colordiff] && $+commands[diff] )); then
    diff(){
        /usr/bin/diff $@ | colordiff
    }
fi
