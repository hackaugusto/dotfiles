#!/usr/bin/env sh

# configure path for xcrun (used by nvim-treesitter)
# this runs in another process, starting it early because it is a large download
xcode-select --install
softwareupdate --install-rosetta

# Install brew.sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# svn is required for font-source-code-pro
# dotnet-sdk openjdk@8 gradle required for dafny
brew install jq tmux semgrep pinentry-mac gpg tig iterm2 showkey svn podman lima scrcpy the_silver_searcher emscripten pre-commit gnuplot graphviz openjdk golang protobuf amethyst skhd yabai dotnet-sdk openjdk@8 gradle minicom arm-none-eabi-gdb openocd sdl2 lsusb qmk-toolbox argocd kubectl postgresql
 
brew install --cask --no-quarantine middleclick alacritty basictex
brew install --cask android-platform-tools

brew tap homebrew/cask-fonts
brew install font-inconsolata font-source-code-pro font-inconsolata-nerd-font

brew tap ethereum/ethereum
brew install solidity

# pinentry-mac has keychain usage enabled by default, disable it
defaults write org.gpgtools.common UseKeychain NO

# disable animations
defaults write NSGlobalDomain NSWindowResizeTime -float 0.001
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false

# nvm is installed via zgen as a plugin to ZSH
nvm install node

# there doesnt seem to be a better way :(
pip install --break-system-packages --user neovim pynvim

# Add tmux to MacOS terminfo database to enable italic support
/opt/homebrew/Cellar/ncurses/6.3/bin/infocmp -x tmux-256color >tmux-256color.src
sed -i '' -e 's/pairs#0x10000/pairs#32767/g' tmux-256color.src
/usr/bin/tic -x -o $HOME/.local/share/terminfo tmux-256color.src
rm ./tmux-256color.src

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add --toolchain stable rust-src rust-docs rust-analyzer rustfmt clippy cargo
rustup component add --toolchain nightly llvm-tools miri rust-src rust-docs rust-analyzer rustfmt clippy cargo

cargo install cargo-edit
