" Some plugins might be CRLF ended, be nice and use LF only:
"
" find ~/.vim -type f -not -path '*/.git/*' -print0
"   | xargs -0 file -N
"   | grep CRLF
"   | cut -d ':' -f 1
"   | parallel --files cat {} \| tr -d '\\r' \| sponge {}

" https://github.com/gmarik/vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'ack.vim'
Bundle 'Align'
Bundle 'bogado/file-line'
" Bundle 'bling/vim-bufferline'
Bundle 'cpp.vim'
" Bundle 'chrisbra/csv.vim'
Bundle 'django.vim'
Bundle 'ekalinin/Dockerfile.vim'
" Bundle 'file:///mnt/extra/code/rust.vim'
Bundle 'groenewege/vim-less'
Bundle 'hackaugusto/javascript.vim'
Bundle 'hackaugusto/vim-tags'
Bundle 'HTML-AutoCloseTag'
Bundle 'indenthtml.vim'
Bundle 'javacomplete'
" Bundle 'jeetsukumaran/vim-markology'
Bundle 'jnwhiteh/vim-golang'
Bundle 'jmcantrell/vim-virtualenv'
Bundle 'kana/vim-operator-user'
Bundle 'kchmck/vim-coffee-script'
Bundle 'leshill/vim-json'
Bundle 'matchit.zip'
Bundle 'mhinz/vim-startify'
Bundle 'mutewinter/nginx.vim'
Bundle 'hail2u/vim-css3-syntax'
Bundle 'mutewinter/vim-tmux'
Bundle 'mhinz/vim-signify'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'netrw.vim'
Bundle 'nono/vim-handlebars'
Bundle 'othree/html5.vim'
"Bundle 'paradigm/vim-multicursor'
Bundle 'php.vim'
Bundle 'prettyprint.vim'
Bundle 'python_match.vim'
Bundle 'python.vim'
Bundle 'rhysd/vim-operator-surround'
Bundle 'scrooloose/syntastic'
" Bundle 'SearchComplete'             " /<Up> not working
Bundle 'Shougo/neocomplete.vim'
Bundle 'Shougo/neomru.vim'
Bundle 'Shougo/unite-help'
Bundle 'Shougo/unite.vim'
" Bundle 'Shougo/unite-outline'
" Bundle 'Shougo/vimproc.vim' - Do not forget to run make in vimproc directory
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/vimshell.vim'
" Bundle 'smarty.vim'
Bundle 'sjl/gundo.vim'
Bundle 'spf13/vim-gocode'
Bundle 'SQLUtilities'
" Bundle 'SuperTab'                   " Replaced by YouCompleteMe
Bundle 'terryma/vim-multiple-cursors'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-endwise'
Bundle 'tsukkee/unite-tag'
Bundle 'vim-scripts/SyntaxComplete'
Bundle 'wavded/vim-stylus'
Bundle 'Kocha/vim-unite-tig'
" Bundle 'kshenoy/vim-signature'

" colortheme
Bundle 'nanotech/jellybeans.vim'
Bundle 'whatyouhide/vim-gotham'
