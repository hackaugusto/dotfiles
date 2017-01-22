ssh-keygen() {
    # uses the OpenSSH new format by default, it should be more resistent to
    # brute-forc attacks
    \ssh-keygen -o $@
}

ssh-add() {
    # respect gpg agent ttl
    if [ -e ~/.gnupg/gpg-agent.conf ]; then
        ttl=$(grep ttl-ssh ~/.gnupg/gpg-agent.conf | head -n1 | awk '{print $2}')

        if [ -n "${$ttl}" ]; then
            ssh-add -t $ttl $@
        else
            ssh-add $@
        fi
    else
        ssh-add $@
    fi
}

ssh(){
    [ -z "${SSH_AUTH_SOCK}" ] && eval $(ssh-agent) >/dev/null 2>&1

    # if we are using gpg-agent this operation will fail
    \ssh-add -l > /dev/null 2>&1
    sshagent=$?

    [ $sshagent -eq 0 ] && \ssh -G $@ | grep -i identityfile | awk '{print $2}' | while read unexpanded_file; do
        file=${unexpanded_file/#\~/$HOME}

        if [ -e $file ]; then
            ssh-add -l | grep $file > /dev/null || ssh-add $file
        fi
    done

    \ssh $@
}
