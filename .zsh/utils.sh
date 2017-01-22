
# https://nicholassterling.wordpress.com/2012/03/30/a-zsh-map-function/
mapa() {
    typeset f="\$[ $1 ]"; shift
    map "$f" "$@"
}

# arch's: /usr/lib/initcpio/functions
map() {
    local r=0
    for _ in "${@:2}"; do
        "$1" "$_" || (( $# > 255 ? r=1 : ++r ))
    done
    return $r
}

if (( $+commands[php] )); then
    function urlencode() { php -r '$s = isset($argv[1]) ? $argv[1] : fgets(STDIN); echo urlencode($s) . "\n";' $@ }
    function urldecode() { python -c "import sys, urllib as ul; print ul.unquote_plus(sys.argv[1])" $1 }
fi

if (( $+commands[python] )); then
    if ! whence _diff > /dev/null; then
        function urlencode() { python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1])" $1 }
    fi
    function format() { python2.7 -c "import sys; print sys.argv[1].format(*(sys.argv[2:] if len(sys.argv) > 2 else sys.stdin))" $@; }
fi

urlencodestream() {
    mapa urlencode
}

lyrics() {
    [ $# -ne 2 ] && echo 'lyrics <bandname> <musicname>' && return 1

    UA='Mozilla/5.0 (X11; Linux x86_64; rv:35.0) Gecko/20100101 Firefox/35.0'
    curl --silent -A$UA http://www.azlyrics.com/lyrics/$1/$2.html | awk '/start of lyrics/,/end of lyrics/ { print $0 }' | sed 's/<[^>]*>//g'

    # curl --silent 'http://letras.mus.br/$1/$2/' | awk '/id="div_letra"/,/id="js-adsenseplayer"/ { print $0 }' | sed 's/<[^>]*>//g'
}

color() {
    [ $# -ne 1 ] && echo "${0} <file>"

    cat ${1} | pygmentize -l ${1/*./} -f html -O noclasses > "${1}.html"
}

git-report() {
    [ $# -ne 1 ] && echo "${0} [author]" && return 1

    git log --author $1 --author-date-order --all --no-merges --date=relative --abbrev-commit "--pretty=format:[%ar] %an %H" --stat
}

slugify() {
    # rely on the fact that this function is defined on my .zshrc
    [ $# -eq 0 ] && xargs -I{} zsh -ic "slugify '{}'" && return

    echo $@ | tr '[:upper:]' '[:lower:]' | sed 's,\s,_,g'
}

cores() {
    awk '/^processor/ {cpu++} END {print cpu}' /proc/cpuinfo
}

pxargs() {
    threads=$(cores)
    serverlist=$1
    shift 1
    xargs -a $serverlist -I"SERVER" -P${threads} -n1 sh -c "$@"
}
