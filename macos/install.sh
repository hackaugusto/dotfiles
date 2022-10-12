#!/usr/bin/env sh

# Install brew.sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

brew install jq tmux semgrep pinentry-mac gpg tig iterm2 showkey alacritty

brew install --cask --no-quarantine middleclick
