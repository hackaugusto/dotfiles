if (( $+commands[npm] )); then
    eval "$(npm completion 2>/dev/null)"
fi

if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh plugins/adb
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/docker-compose

    # web
    zgen oh-my-zsh plugins/bower
    zgen oh-my-zsh plugins/grunt
    zgen oh-my-zsh plugins/httpie

    # python
    zgen oh-my-zsh plugins/python
    zgen oh-my-zsh plugins/paver
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/celery
    zgen oh-my-zsh plugins/django
    zgen oh-my-zsh plugins/fabric

    zgen oh-my-zsh plugins/pylint
    zgen oh-my-zsh plugins/pep8
    zgen oh-my-zsh plugins/autopep8

    # zgen oh-my-zsh plugins/scala
    # zgen oh-my-zsh plugins/sbt
    # zgen oh-my-zsh plugins/ant
    # zgen oh-my-zsh plugins/lein
    # zgen oh-my-zsh plugins/brew
    # zgen oh-my-zsh plugins/capistrano
    # zgen oh-my-zsh plugins/gradle

    zgen load kennethreitz/autoenv
    zgen load StackExchange/blackbox
    zgen load zsh-users/zsh-completions
    zgen load zsh-users/zsh-syntax-highlighting
    zgen load rust-lang/zsh-config
    # zgen load vifon/deer

    zgen save
fi
