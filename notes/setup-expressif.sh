#!/usr/bin/zsh

export ESP_ROOT=$(pwd)/esp
export ESP_TOOLCHAIN=$ESP_ROOT/toolchain
export ESP_VIRTUALENV=$ESP_ROOT/venv
export IDF_PATH=$ESP_ROOT/idf  # constant used by the IDF

export ESP_TOOLCHAIN_URL="https://dl.espressif.com/dl/xtensa-esp32-elf-linux64-1.22.0-80-g6c4433a-5.2.0.tar.gz"
export ESP_IDF_VERSION="v3.3"
export ESP_IDF_GIT_URL="https://github.com/espressif/esp-idf.git"
export ESP_IDF_DOCUMENTATION_URL="https://readthedocs.com/projects/espressif-esp-idf/downloads/pdf/release-${ESP_IDF_VERSION}/"

msg() {
    printf "$(tput bold)$(tput setaf 2) $1$(tput sgr0)\n"
}

error () {
    printf "$(tput bold)$(tput setaf 1)$1$(tput sgr0)\n" >&2
}

[[ $ZSH_EVAL_CONTEXT =~ :file$ ]] || error """
It looks like the script is not being sorced, this won't properly set the path
to use the ESP toolchain, please source the script instead of executing it.
""";

lsmod | grep cp210x > /dev/null 2>&1 || {
    error "The kernel module cp210x is not loaded. Bailing!"
    return 1
}

groups | grep ucp > /dev/null 2>&1 || {
    error "Current user is not part of the uucp group, this group is required to use the UART interface."
    return 1
}

[ ! -e $IDF_PATH ] && {
    msg "Cloning IDF repo"
    # A shallow copy `--depth 1` will not work properly because one of the IDF
    # tools will try to run `git describe` during its setup.py
    git clone --recursive $ESP_IDF_GIT_URL -b release/$ESP_IDF_VERSION $IDF_PATH
} || {
    msg "IDF repo is already set"

    (
        cd $IDF_PATH
        git fetch --tags --all
        git checkout "release/$ESP_IDF_VERSION"
        git submodule update
    )
}

[ ! -e $ESP_TOOLCHAIN ] && {
    msg "Downloading the precompiled toolchain"
    (
        mkdir -p $ESP_TOOLCHAIN
        cd $ESP_TOOLCHAIN
        curl --silent $ESP_TOOLCHAIN_URL | tar -xzf -
    )
} || {
    msg "Precompiled Toolchain is already downloaded"
}

[ ! -e $ESP_ROOT/idf-$ESP_IDF_VERSION-doc.pdf ] && {
    wget $ESP_IDF_DOCUMENTATION_URL -O $ESP_ROOT/idf-$ESP_IDF_VERSION-doc.pdf
}

[ ! -e $ESP_VIRTUALENV ] && {
    msg "Setting up the python2 venv"
    # IDF requires python2
    virtualenv2 $ESP_VIRTUALENV
    pip install -r $IDF_PATH/requirements.txt
} || {
    msg "python2 venv is already set"
}

source $ESP_VIRTUALENV/bin/activate

# make sure all the tools are available in the PATH
export PATH="$ESP_TOOLCHAIN/xtensa-esp32-elf/bin:$IDF_PATH/tools:$PATH"
