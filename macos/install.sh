#!/usr/bin/env sh

# Install brew.sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"


# svn is required for font-source-code-pro
brew install jq tmux semgrep pinentry-mac gpg tig iterm2 showkey svn

brew install --cask --no-quarantine middleclick alacritty

brew tap homebrew/cask-fonts
brew install font-inconsolata font-source-code-pro
