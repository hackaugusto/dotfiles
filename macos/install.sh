#!/usr/bin/env sh

# configure path for xcrun (used by nvim-treesitter)
# this runs in another process, starting it early because it is a large download
xcode-select --install

# Install brew.sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# svn is required for font-source-code-pro
brew install jq tmux semgrep pinentry-mac gpg tig iterm2 showkey svn podman lima scrcpy

brew install --cask --no-quarantine middleclick alacritty basictex
brew install --cask android-platform-tools

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

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add llvm-tools miri rust-src rust-docs rust-analyzer rustfmt clippy cargo
