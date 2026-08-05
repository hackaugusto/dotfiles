#!/usr/bin/env sh

# configure path for xcrun (used by nvim-treesitter)
# this runs in another process, starting it early because it is a large download
xcode-select --install
softwareupdate --install-rosetta

# Install brew.sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add --toolchain stable rust-src rust-docs rust-analyzer rustfmt clippy cargo
rustup component add --toolchain nightly llvm-tools miri rust-src rust-docs rust-analyzer rustfmt clippy cargo

# svn is required for font-source-code-pro
# dotnet-sdk openjdk@8 gradle required for dafny
brew install jq tmux semgrep pinentry-mac gpg tig iterm2 showkey svn podman lima scrcpy the_silver_searcher emscripten pre-commit gnuplot graphviz openjdk golang protobuf dotnet-sdk gradle minicom arm-none-eabi-gdb openocd sdl2 lsusb qmk-toolbox argocd kubectl postgresql kubernetes-cli hugo minikube kubectl kubectx helm helmfile gh fzf watch black isort ripgrep goenv temurin
brew install --cask claude-code
brew install hashicorp/tap/terraform
brew install hashicorp/tap/vault

brew install pyenv
pyenv install 3.14
pyenv global 3.14
pip3 install --user neovim pynvim

brew install --cask gcloud-cli
gcloud components update

cargo install --git https://github.com/tauri-apps/cargo-mobile2
brew install --cask --no-quarantine middleclick alacritty basictex
brew install --cask android-platform-tools librewolf
brew install --cask ovh/tap/ovhcloud-cli
brew install --cask ghostty

brew tap homebrew/cask-fonts
brew install font-inconsolata font-source-code-pro font-inconsolata-nerd-font

brew tap ethereum/ethereum
brew install solidity

brew install openjdk@17
sudo ln -sfn /opt/homebrew/opt/openjdk@17/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-17.jdk
brew install openjdk@21
sudo ln -sfn /opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk-21.jdk

# caps as ctrl
ln -s $(realpath ./Library/LaunchAgents/com.local.keyremap.plist) ${HOME}/Library/LaunchAgents/com.local.keyremap.plist
launchctl load ~/Library/LaunchAgents/com.local.keyremap.plist

# pinentry-mac has keychain usage enabled by default, disable it
defaults write org.gpgtools.common UseKeychain NO

# disable animations
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false

# nvm is installed via zgen as a plugin to ZSH
nvm install node

# Add tmux to MacOS terminfo database to enable italic support
/opt/homebrew/Cellar/ncurses/6.3/bin/infocmp -x tmux-256color >tmux-256color.src
sed -i '' -e 's/pairs#0x10000/pairs#32767/g' tmux-256color.src
/usr/bin/tic -x -o $HOME/.local/share/terminfo tmux-256color.src
rm ./tmux-256color.src

cargo install cargo-edit
