
aurdl() {
    local cwd destination package

    cwd=$(pwd)
    destination=${BUILDDIR:-$PWD}
    for package in ${@##-*}; do
        cd "$destination"
        curl "https://aur.archlinux.org/cgit/aur.git/snapshot/$package.tar.gz" | tar xz
        cd "$package"
        makepkg ${@##[^\-]*}
    done
    cd $cwd
}
