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
  Plugin 'hackaugusto/Vundle.vim'

  " AUTOCOMPLETE
  " Plugin 'Shougo/neocomplete.vim'
  " Plugin 'Valloric/YouCompleteMe'         " installed through pacman
  Plugin 'davidhalter/jedi-vim'             " ycm installs the jedi library, not the vim plugin
                                            " using function signature and jumping around (could use :YcmCompleter GoToDefinition)
  " Plugin 'ervandew/supertab'              " supertab conflicts with ultisnips and YouCompleteMe superseeds it
  Plugin 'racer-rust/vim-racer'

  " snippets
  Plugin 'SirVer/ultisnips'                 " snippet plugin
  Plugin 'honza/vim-snippets'               " the snippets library

  " Plugin 'paradigm/vim-multicursor'
  Plugin 'terryma/vim-multiple-cursors'

  " colortheme
  Plugin 'nanotech/jellybeans.vim'
  Plugin 'whatyouhide/vim-gotham'
  Plugin 'junegunn/seoul256.vim'

  " presentation
  Plugin 'junegunn/goyo.vim'
  Plugin 'junegunn/limelight.vim'

  " search
  " Plugin 'SearchComplete'                 " /<Up> not working
  Plugin 'ack.vim'
  Plugin 'junegunn/fzf'
  Plugin 'junegunn/fzf.vim'
  Plugin 'junegunn/vim-oblique'
  Plugin 'junegunn/vim-pseudocl'

  Plugin 'hackaugusto/vim-tags'
  Plugin 'jmcantrell/vim-virtualenv'
  Plugin 'mhinz/vim-startify'               " Show latest files
  Plugin 'bogado/file-line'                 " Opens the file at the line `vim file:<line>`

  Plugin 'junegunn/vim-easy-align'
  Plugin 'kana/vim-operator-user'
  Plugin 'Kocha/vim-unite-tig'
  Plugin 'matchit.zip'
  Plugin 'mhinz/vim-signify'
  Plugin 'rhysd/vim-operator-surround'
  Plugin 'scrooloose/syntastic'
  Plugin 'Shougo/neomru.vim'
  Plugin 'Shougo/unite-help'
  Plugin 'Shougo/unite.vim'
  Plugin 'Shougo/vimproc.vim'               " Do not forget to run make in vimproc directory
  Plugin 'tpope/vim-commentary'
  Plugin 'tpope/vim-dispatch'
  Plugin 'tpope/vim-endwise'
  Plugin 'tpope/vim-fugitive'
  Plugin 'tpope/vim-markdown'
  Plugin 'tpope/vim-repeat'
  Plugin 'tpope/vim-surround'
  " Plugin 'tpope/vim-unimpaired'           " rolled my own version, also this is conflicting with python.vim
  Plugin 'tsukkee/unite-tag'
  Plugin 'visualrepeat'

  " highlight and file specific
  Plugin 'chase/vim-ansible-yaml'
  Plugin 'dccmx/vim-lemon-syntax'
  Plugin 'django.vim'
  Plugin 'hackaugusto/javascript.vim'
  Plugin 'hail2u/vim-css3-syntax'
  Plugin 'HTML-AutoCloseTag'
  Plugin 'jeaye/color_coded'
  Plugin 'jneen/ragel.vim'
  Plugin 'nathanaelkane/vim-indent-guides'
  Plugin 'netrw.vim'
  Plugin 'prettyprint.vim'
  Plugin 'python_match.vim'
  Plugin 'python.vim'                     " blocks movements (function, class, etc)
  Plugin 'sheerun/vim-polyglot'
  Plugin 'sjl/gundo.vim'
  Plugin 'spf13/vim-gocode'
  Plugin 'SQLUtilities'
  Plugin 'vim-scripts/SyntaxComplete'
call vundle#end()
filetype plugin indent on
