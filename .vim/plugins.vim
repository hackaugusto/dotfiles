" Some plugins might be CRLF ended, be nice and use LF only:
"
" find ~/.vim -type f -not -path '*/.git/*' -print0
"   | xargs -0 file -N
"   | grep CRLF
"   | cut -d ':' -f 1
"   | parallel --files cat {} \| tr -d '\\r' \| sponge {}

" git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim/

" Alternative to vundle#rc, offers speed up by modifying rtp only when end()
" called later.
call vundle#begin()
  Plugin 'gmarik/Vundle.vim'

  " AUTOCOMPLETE
  " Plugin 'SearchComplete'                 " /<Up> not working
  " Plugin 'Shougo/neocomplete.vim'
  " Plugin 'Valloric/YouCompleteMe'         " installed through pacman
  Plugin 'davidhalter/jedi-vim'             " used for navigation
  Plugin 'SirVer/ultisnips'
  Plugin 'honza/vim-snippets'
  Plugin 'SuperTab'

  Plugin 'ack.vim'
  " this plugin adds to many mappings
  " Plugin 'Align'
  " Plugin 'amitdev/vimpy'                  " not a good plugin
  Plugin 'bogado/file-line'
  " Plugin 'bling/vim-bufferline'
  Plugin 'cpp.vim'
  Plugin 'chase/vim-ansible-yaml'
  " Plugin 'chrisbra/csv.vim'
  Plugin 'django.vim'
  Plugin 'ekalinin/Dockerfile.vim'
  " Plugin 'file:///mnt/extra/code/rust.vim'
  Plugin 'groenewege/vim-less'
  Plugin 'hackaugusto/javascript.vim'
  Plugin 'hackaugusto/vim-tags'
  Plugin 'HTML-AutoCloseTag'
  Plugin 'indenthtml.vim'
  " Plugin 'javacomplete'
  " Plugin 'jeetsukumaran/vim-markology'
  Plugin 'jnwhiteh/vim-golang'
  Plugin 'jmcantrell/vim-virtualenv'
  Plugin 'kana/vim-operator-user'
  Plugin 'kchmck/vim-coffee-script'
  Plugin 'leshill/vim-json'
  Plugin 'matchit.zip'
  Plugin 'mhinz/vim-startify'
  Plugin 'mutewinter/nginx.vim'
  Plugin 'hail2u/vim-css3-syntax'
  Plugin 'mutewinter/vim-tmux'
  Plugin 'mhinz/vim-signify'
  Plugin 'nathanaelkane/vim-indent-guides'
  Plugin 'netrw.vim'
  Plugin 'nono/vim-handlebars'
  Plugin 'othree/html5.vim'
  "Plugin 'paradigm/vim-multicursor'
  Plugin 'php.vim'
  Plugin 'prettyprint.vim'
  Plugin 'python_match.vim'
  Plugin 'python.vim'
  " Plugin 'Raimondi/delimitMate'
  Plugin 'rhysd/vim-operator-surround'
  Plugin 'scrooloose/syntastic'
  Plugin 'Shougo/neomru.vim'
  Plugin 'Shougo/unite-help'
  Plugin 'Shougo/unite.vim'
  " Plugin 'Shougo/unite-outline'
  Plugin 'Shougo/vimproc.vim'               " Do not forget to run make in vimproc directory
  Plugin 'Shougo/vimshell.vim'
  " Plugin 'smarty.vim'
  Plugin 'sjl/gundo.vim'
  Plugin 'spf13/vim-gocode'
  Plugin 'SQLUtilities'
  Plugin 'terryma/vim-multiple-cursors'
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-dispatch'
  Plugin 'tpope/vim-fugitive'
  Plugin 'tpope/vim-haml'
  Plugin 'tpope/vim-markdown'
  Plugin 'tpope/vim-repeat'
  Plugin 'tpope/vim-surround'
  Plugin 'tpope/vim-unimpaired'
  Plugin 'tpope/vim-endwise'
  Plugin 'tsukkee/unite-tag'
  Plugin 'vim-scripts/SyntaxComplete'
  Plugin 'wavded/vim-stylus'
  Plugin 'Kocha/vim-unite-tig'
  " Plugin 'kshenoy/vim-signature'

  " colortheme
  Plugin 'nanotech/jellybeans.vim'
  Plugin 'whatyouhide/vim-gotham'

call vundle#end()
filetype plugin indent on
