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
