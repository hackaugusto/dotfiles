#!/usr/bin/env sh

# Install brew.sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# svn is required for font-source-code-pro
brew install jq tmux semgrep pinentry-mac gpg tig iterm2 showkey svn podman lima

brew install --cask --no-quarantine middleclick alacritty

brew tap homebrew/cask-fonts
brew install font-inconsolata font-source-code-pro

# nvm is installed via zgen as a plugin to ZSH
nvm install node

# there doesnt seem to be a better way :(
pip install --user neovim

# Add tmux to MacOS terminfo database to enable italic support
/opt/homebrew/Cellar/ncurses/6.3/bin/infocmp -x tmux-256color >tmux-256color.src
sed -i '' -e 's/pairs#0x10000/pairs#32767/g' tmux-256color.src
/usr/bin/tic -x -o $HOME/.local/share/terminfo tmux-256color.src
rm ./tmux-256color.src
