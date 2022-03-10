source ~/.zgen/zgen.zsh

# TODO: why we need to call it twice?
zgen saved

if ! zgen saved; then
    echo "Creating a zgen save"

    # zsh
    zgen load zsh-users/zsh-completions
    zgen load zsh-users/zsh-syntax-highlighting

    # utilities
    # zgen load zdharma-continuum/zsh-diff-so-fancy

    # python (not available in zsh-completions as of 10/Mar/22)
    zgen oh-my-zsh plugins/pep8
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/pipenv
    zgen oh-my-zsh plugins/pylint

    # other languages
    zgen load lukechilds/zsh-nvm
    # zgen oh-my-zsh plugins/rust
    zgen oh-my-zsh plugins/adb
    zgen oh-my-zsh plugins/docker
    zgen oh-my-zsh plugins/docker-compose

    zgen save
fi
