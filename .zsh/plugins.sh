if (( $+commands[npm] )); then
    eval "$(npm completion 2>/dev/null)"
fi

if ! zgen saved; then
    echo "Creating a zgen save"

    zgen oh-my-zsh plugins/adb
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/docker-compose

    zgen oh-my-zsh plugins/extract

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

    # TODO: PR for zgen support
    #
    # pyenv install 2.7.8
    # pyenv install 3.5.1
    # VERSION_ALIAS="2.7.8-debug" PYTHON_CONFIGURE_OPTS="--with-pydebug" pyenv install 2.7
    # VERSION_ALIAS="3.5.1-debug" PYTHON_CONFIGURE_OPTS="--with-pydebug" pyenv install 3.5.1
    #
    # git clone https://github.com/haypo/pytracemalloc.git
    # cat pytracemalloc/patches/2.7/pep445.patch | filterdiff --strip=1 | VERSION_ALIAS="2.7.8-trace" PYTHON_CONFIGURE_OPTS="--with-pydebug --enable-shared --enable-unicode=ucs4" pyenv install -p -v 2.7.8
    zgen load yyuu/pyenv
    git clone https://github.com/s1341/pyenv-alias.git ~/.pyenv/plugins/pyenv-alias
    git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv

    zgen save
fi
