" Some plugins might be CRLF ended, be nice and use LF only:
"
" find ~/.vim -type f -not -path '*/.git/*' -print0
"   | xargs -0 file -N
"   | grep CRLF
"   | cut -d ':' -f 1
"   | parallel --files cat {} \| tr -d '\\r' \| sponge {}

" Managers:
"   - https://github.com/junegunn/vim-plug
"     Setup:
"        curl -fLo ~/.vim/autoload/plug.vim --create-dirs 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
"
"   - https://github.com/Shougo/neobundle.vim
"
"   - https://github.com/VundleVim/Vundle.vim
"     Setup:
"        git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
"        set nocompatible
"        filetype off
"        set rtp+=~/.vim/bundle/Vundle.vim/
"     Notes:
"       vundle prepends vim-scripts
"       [https://github.com/VundleVim/Vundle.vim/blob/2506347586621114ccb6fedb7f93a560fc19c691/autoload/vundle/config.vim#L156]
"
"   - https://github.com/tpope/vim-pathogen
"
" Plugins that have problemas:
"  amitdev/vimpy
"  bling/vim-bufferline
"  chrisbra/csv.vim
"  jeetsukumaran/vim-markology
"  paradigm/vim-multicursor
"  Raimondi/delimitMate
"  Shougo/unite-outline
"  smarty.vim
"  kshenoy/vim-signature
"  Shougo/vimshell.vim
"  vim-scripts/indenthtml.vim
"  vims-scripts/javacomplete

call plug#begin('~/.vim/bundle')
  " appearence/colortheme
  Plug 'nanotech/jellybeans.vim'
  Plug 'whatyouhide/vim-gotham'
  Plug 'junegunn/limelight.vim'
  Plug 'mhinz/vim-startify'

  " Autocomplete:
  " Plug 'Shougo/neocomplete.vim'
  " Plug 'Valloric/YouCompleteMe' " installed through AUR

  " Search:
  " Plug 'SearchComplete' " /<Up> not working

  " Code:
  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'
  Plug 'scrooloose/syntastic'
  Plug 'vim-scripts/SyntaxComplete'

  " Utils:
  Plug 'vim-scripts/ack.vim'
  Plug 'vim-scripts/Align'
  Plug 'vim-scripts/matchit.zip'
  Plug 'vim-scripts/netrw.vim'
  Plug 'vim-scripts/SuperTab'
  Plug 'vim-scripts/prettyprint.vim'
  Plug 'hackaugusto/vim-tags'
  Plug 'bogado/file-line'
  Plug 'kana/vim-operator-user'
  Plug 'mhinz/vim-signify'
  Plug 'nathanaelkane/vim-indent-guides'
  Plug 'rhysd/vim-operator-surround'
  Plug 'Shougo/neomru.vim'
  Plug 'Shougo/unite-help'
  Plug 'Shougo/unite.vim'
  Plug 'Shougo/vimproc.vim',            {'do': 'make'}
  Plug 'sjl/gundo.vim'
  Plug 'spf13/vim-gocode'
  Plug 'terryma/vim-multiple-cursors'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-dispatch'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-haml'
  Plug 'tpope/vim-repeat'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-unimpaired'
  Plug 'tpope/vim-endwise'
  Plug 'tsukkee/unite-tag'
  Plug 'Kocha/vim-unite-tig'

  " Languages:
  Plug 'vim-scripts/cpp.vim'
  Plug 'vim-scripts/php.vim'
  Plug 'rust-lang/rust.vim'
  Plug 'chase/vim-ansible-yaml'
  Plug 'ekalinin/Dockerfile.vim'
  Plug 'groenewege/vim-less'
  Plug 'hackaugusto/javascript.vim'
  Plug 'jnwhiteh/vim-golang'
  Plug 'leshill/vim-json'
  Plug 'kchmck/vim-coffee-script'
  Plug 'mutewinter/nginx.vim'
  Plug 'hail2u/vim-css3-syntax'
  Plug 'mutewinter/vim-tmux'
  Plug 'nono/vim-handlebars'
  Plug 'tpope/vim-markdown'
  Plug 'wavded/vim-stylus'
  Plug 'othree/html5.vim'

  Plug 'vim-scripts/python.vim',      {'for': 'python'}
  Plug 'vim-scripts/django.vim',      {'for': 'python'}
  Plug 'vim-scripts/python_match.vim',{'for': 'python'}
  Plug 'davidhalter/jedi-vim',        {'for': 'python'}
  Plug 'jmcantrell/vim-virtualenv'

  Plug 'vim-scripts/HTML-AutoCloseTag', {'for': 'html'}
  Plug 'vim-scripts/SQLUtilities',      {'for': 'sql' }

call plug#end()
