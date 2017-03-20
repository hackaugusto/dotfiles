# TODO: why we need to call it twice?
zgen saved

if ! zgen saved; then
    echo "Creating a zgen save"

    # zsh
    zgen load zsh-users/zsh-completions
    zgen load zsh-users/zsh-syntax-highlighting

    # utilities
    zgen load StackExchange/blackbox
    zgen load Tarrasch/zsh-autoenv
    zgen oh-my-zsh plugins/extract
    # zgen oh-my-zsh plugins/gitfast

    # web
    # zgen oh-my-zsh plugins/bower
    # zgen oh-my-zsh plugins/grunt
    # zgen oh-my-zsh plugins/httpie

    # python
    # zgen load yyuu/pyenv
    zgen load hackaugusto/pyenv
    zgen oh-my-zsh plugins/autopep8
    zgen oh-my-zsh plugins/celery
    zgen oh-my-zsh plugins/fabric
    zgen oh-my-zsh plugins/pep8
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/python
    # zgen oh-my-zsh plugins/paver
    # zgen oh-my-zsh plugins/pylint
    # zgen oh-my-zsh plugins/django

    # other languages
    zgen load lukechilds/zsh-nvm
    zgen load rust-lang/zsh-config
    zgen oh-my-zsh plugins/adb
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/docker-compose
    # zgen oh-my-zsh plugins/scala
    # zgen oh-my-zsh plugins/sbt
    # zgen oh-my-zsh plugins/ant
    # zgen oh-my-zsh plugins/lein
    # zgen oh-my-zsh plugins/brew
    # zgen oh-my-zsh plugins/capistrano
    # zgen oh-my-zsh plugins/gradle

    zgen save
fi
