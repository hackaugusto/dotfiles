if (( ! $+commands[ack] && $+commands[ack-grep] )); then
    alias ack='ack-grep';
fi

if (( $+commands[wdiff] )); then
    if (( $+commands[cwdiff] )); then
        diff(){
            /usr/bin/wdiff $@ | awk '/\[-/,/-\]/; /{\+/,/\+}/' | cwdiff -f
            return $pipestatus[1]
        }
    else
        diff(){
            /usr/bin/wdiff $@ | awk '/\[-/,/-\]/; /{\+/,/\+}/'
            return $pipestatus[1]
        }
    fi
elif (( $+commands[colordiff] && $+commands[diff] )); then
    diff(){
        /usr/bin/diff $@ | colordiff
        return $pipestatus[1]
    }
fi
