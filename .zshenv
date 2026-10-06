. "$HOME/.cargo/env"

export PATH="$PATH:/Users/hack/.foundry/bin"
export PATH="$PATH:$HOME/.elan/bin"
. "$HOME/.cargo/env"

if [[ "$OSTYPE" = darwin* ]]; then
    function deduplicate_path() {
        # split the path, remove duplicates and re-add at the end
        IFS=':'
        duplicates=$@
        echo "${${=PATH}:|duplicates}:${duplicates}"
    }

    eval "$(/opt/homebrew/bin/brew shellenv)"
    export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"

    export PATH="$(deduplicate_path '/sbin' '/bin' '/usr/bin'):/Users/hack/.antigravity/antigravity/bin"
    export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
    export BROWSER=/Applications/Firefox.app/Contents/MacOS/firefox

    export ANDROID_HOME="$(brew --prefix)/share/android-commandlinetools"
    export ANDROID_NDK_HOME="$ANDROID_HOME/ndk/27.2.12479018"

    export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
else
    export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
fi
