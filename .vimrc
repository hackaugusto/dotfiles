" Vundle - https://github.com/gmarik/vundle
" required!
set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" required! 
Bundle 'gmarik/vundle' 

" some plugins might be CRLF ended, be nice and use LF only:
"
" find ~/.vim -type f -not -path '*/.git/*' -print0
"   | xargs -0 file -N
"   | grep CRLF
"   | cut -d ':' -f 1
"   | parallel --files cat {} \| tr -d '\\r' \| sponge {}
"
Bundle 'cpp.vim'
Bundle 'php.vim'
Bundle 'smarty.vim'
Bundle 'python.vim'
Bundle 'django.vim'
" Bundle 'haskell.vim'
Bundle 'javacomplete'
Bundle 'SQLComplete.vim'
" Bundle 'man.vim'
" Bundle 'OmniCppComplete'
" Bundle 'pythoncomplete'

Bundle 'python_match.vim'
Bundle 'Align'
Bundle 'checksyntax'
Bundle 'HTML-AutoCloseTag'
Bundle 'matchit.zip'
Bundle 'prettyprint.vim'
Bundle 'SearchComplete'
Bundle 'netrw.vim'
Bundle 'SuperTab'
Bundle 'SQLUtilities'
Bundle 'indenthtml.vim'
" Bundle 'fly.vim'

" (needs compilation)
" requires clang and jedi
" Bundle 'Valloric/YouCompleteMe' " installed the AUR package
"
" YouCompleteMe comes with jedi
" Bundle 'davidhalter/jedi'
" Bundle 'digitaltoad/vim-jade'

Bundle 'bogado/file-line'
Bundle 'groenewege/vim-less'
Bundle 'hallettj/jslint.vim'
Bundle 'jnwhiteh/vim-golang'
Bundle 'kchmck/vim-coffee-script'
Bundle 'leshill/vim-json'
Bundle 'mutewinter/nginx.vim'
Bundle 'mutewinter/vim-css3-syntax'
Bundle 'mutewinter/vim-tmux'
Bundle 'nono/vim-handlebars'
Bundle 'othree/html5.vim'
Bundle 'paradigm/vim-multicursor'
Bundle 'spf13/vim-gocode'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/SyntaxComplete'
Bundle 'wavded/vim-stylus'

" Bundle 'scrooloose/syntastic' -- Added `flake8-python2` into 
Bundle 'hackaugusto/syntastic'
" Bundle 'vim-scripts/javascript.vim' -- fixed windows line ending
Bundle 'hackaugusto/javascript.vim'
" Bundle 'szw/vim-tags' -- added upwards search for `g:vim_tags_directories`
Bundle 'hackaugusto/vim-tags'

Bundle 'file:///mnt/extra/code/rust.vim'

" required!
filetype plugin indent on     

" General stuff
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set backup swapfile updatetime=20000 updatecount=200
" allow to backscape over anything within insert mode
set backspace=indent,eol,start
set noerrorbells novisualbell
set ttyfast showcmd hidden
set hlsearch
set wildmenu
set modeline
set nocompatible
set relativenumber number
"set encoding=utf-8 fileencoding=utf-8
" `;` is for upward searching, for more info `:help file-searching`
set tags=./tags,tags;

" netrw
let g:netrw_browse_split=3 " open new files one a new tab

" let g:checksyntax = -1 " dont load checksyntax, it blinks the screen while saving php files
" let g:syntastic_debug = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['ruby', 'php', 'python'],
                           \ 'passive_filetypes': [] }

highlight clear TabSel 
highlight clear TabFill
highlight clear TabLine 
highlight clear TabLineSel
highlight clear TabLineFill
highlight TabLineSel cterm=bold
highlight TabLine ctermfg=Cyan cterm=NONE
highlight clear StatusLine
highlight clear StatusLineNC

" set t_Co=16
set t_Co=256
set background=dark
"let g:solarized_termcolors=16
"colorscheme solarized

"set statusline=\ \%f%m%r%h%w\ ::\ %y\ [%{&ff}]\%=\ [%p%%:\ %l/%L]\ 
"set rtp+=/usr/lib/python2.7/site-packages/powerline/bindings/vim/plugin/powerline.vim
set laststatus=2

map <Up> k
map <Down> j
map <Left> :bnext<CR>
map <Right> :bprev<CR>
" map <C-'> ciw'<C-r>"'<Esc>
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

" jk is faster to exit insert mode 
inoremap jk <esc>

syntax on
if has('autocmd')
  filetype on
  filetype plugin on
  filetype indent on
endif

if !exists("autocmd_latex")
  autocmd BufNewFile,BufRead tex let autocmd_latex=1
  autocmd BufNewFile,BufRead tex setlocal textwidth=80
  autocmd BufNewFile,BufRead tex let g:tex_flavor = "latex"
endif

augroup Lisp
  autocmd!
  autocmd FileType lisp set showmatch
augroup END

augroup Css
  autocmd!
  autocmd FileType css set omnifunc=csscomplete#CompleteCSS
augroup END

" augroup SH
"   autocmd!
"   autocmd FileType sh set number
" augroup END

augroup Zsh 
  autocmd!
  autocmd FileType zsh set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
augroup END

augroup Vim
  autocmd!
  autocmd FileType vim set shiftwidth=2 softtabstop=2 tabstop=2 expandtab
augroup END

augroup JAVASCRIPT
  autocmd!
  " autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType javascript set shiftwidth=4 softtabstop=4 tabstop=4
  autocmd FileType javascript set cindent
  autocmd FileType javascript let g:JSLintHighlightErrorLine = 0
augroup END

augroup CSHARP
  autocmd!
  autocmd FileType cs set shiftwidth=4 softtabstop=4 tabstop=4
  autocmd FileType cs set cindent
augroup END

augroup HASKELL
  autocmd!
  autocmd FileType haskell set shiftwidth=4 softtabstop=4 tabstop=4
augroup END

augroup XML
  autocmd!
  autocmd FileType xml set shiftwidth=2 softtabstop=2 tabstop=2 expandtab nocindent noautoindent
  "autocmd FileType xml source ~/.vim/ftplugin/html_autoclosetag.vim
  "autocmd FileType xhtml,html source ~/.vim/ftplugin/closetag.vim
augroup END

augroup JADE
  autocmd!
  autocmd FileType jade set shiftwidth=2 softtabstop=2 tabstop=2 expandtab nocindent noautoindent
augroup END

augroup Cobra
  autocmd!
  autocmd FileType cobra set shiftwidth=4 softtabstop=4 tabstop=4 expandtab nocindent noautoindent
augroup END

augroup HTMLDJANGO
  autocmd!
  autocmd FileType htmldjango set shiftwidth=2 softtabstop=2 tabstop=2

  autocmd FileType htmldjango set omnifunc=htmlcomplete#CompleteTags
  autocmd FileType htmldjango let g:SuperTabDefaultCompletionType='context'
augroup END

augroup HTML
  autocmd!
  autocmd FileType xhtml,html let g:SuperTabDefaultCompletionType='context'
  autocmd FileType htmldjango set omnifunc=htmlcomplete#CompleteTags
  autocmd FileType xhtml,html set shiftwidth=2 softtabstop=2 tabstop=2

" autocmd FileType xhtml,html so ~/.vim/ftplugin/html_autoclosetag.vim
" autocmd FileType xhtml,html so ~/.vim/ftplugin/closetag.vim
augroup END

augroup PHP
  autocmd!
  autocmd FileType php set cindent
  autocmd FileType php let php_sql_query = 1
  autocmd FileType php let php_htmlInStrings = 1
  autocmd FileType php let php_smart_members = 1
augroup END

augroup Java
  autocmd!
  autocmd FileType java set cindent expandtab
  autocmd FileType java set complete=.,i,d omnifunc=javacomplete#Complete 
  autocmd FileType java let g:SuperTabDefaultCompletionType='context'
  autocmd FileType java set shiftwidth=4 softtabstop=4 tabstop=4 

  "autocmd BufRead *.java set efm=%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%#
  "autocmd BufRead set makeprg=ant\ -find\ build.xml
augroup END

function ConfigureSyntastic(type)
  if a:type == 'python' && has('python')
    let builtins = system("python2 -c 'print(\",\".join(dir(__builtins__) + [\"__name__\", \"__file__\", \"__loader__\", \"__package__\", \"__main__\"]))'")
    let g:syntastic_python_flake8_post_args = " --builtins=".builtins
  endif
endfunction

function FindDjangoSettings()
  if strlen($VIRTUAL_ENV) && has('python')
    let output  = system("find $VIRTUAL_ENV \\( -wholename '*/lib/*' -or -wholename '*/install/' \\) -or \\( -name 'settings.py' -print0 \\) | tr '\n' ' '")
    let outarray= split(output, '[\/]\+')
    let module  = outarray[-2] . '.' . 'settings'
    let syspath = system("python -c 'import sys; print sys.path' | tr '\n' ' ' ")
    " let curpath = '/' . join(outarray[:-2], '/')

    execute 'python import sys, os'
    " execute 'python sys.path.append("' . curpath . '")'
    " execute 'python sys.path.append("' . syspath . '")'
    execute 'python sys.path = ' . syspath
    execute 'python os.environ.setdefault("DJANGO_SETTINGS_MODULE", "' . module . '")'
  endif
endfunction

augroup Python
  autocmd!
  autocmd FileType python abbreviate #i import
  autocmd FileType python map @i ^i#i
  autocmd FileType python set omnifunc=pythoncomplete#Complete completeopt-=preview
  autocmd FileType python set expandtab nowrap shiftwidth=4 softtabstop=4
  autocmd FileType python let g:SuperTabDefaultCompletionType='context'
  autocmd FileType python let python_highlight_all=1
  " autocmd FileType python let g:flake8_builtins="_,apply"
  autocmd FileType python call FindDjangoSettings()
  autocmd BufWritePre *.py :%s/\s\+$//e
  autocmd BufReadPost *.py call ConfigureSyntastic('python')
  " autocmd BufWritePost *.py call Flake8()
  " autocmd FileType python let ropevim_vim_completion=1
  " autocmd FileType python let ropevim_extended_complete = 1
  " autocmd FileType python let g:ropevim_autoimport_modules = ["os.*", "django.*"]
  " autocmd FileType python imap <c-space> <C-R>=RopeCodeAssistInsertMode()<CR>
augroup END

augroup Perl
  autocmd!
  autocmd FileType perl set autoindent smartindent cindent
  autocmd FileType perl set shiftwidth=4 softtabstop=4 tabstop=4
  autocmd FileType perl set makeprg=perl\ -c\ %\ $* errorformat=%f:%l:%m
  autocmd FileType perl let perl_include_pod=1
  autocmd FileType perl let perl_extended_vars=1
augroup END

augroup C-Files
  autocmd!
  autocmd FileType cpp,c,h set cindent
  autocmd FileType cpp,c,h set complete=.,i,d omnifunc=ccomplete#Complete
  autocmd FileType cpp,c,h let g:SuperTabDefaultCompletionType='context'
  autocmd FileType cpp,c,h set cscopetag cscopetagorder=0

  autocmd FileType cpp,c,h abbreviate #d #define
  autocmd FileType cpp,c,h abbreviate #i #include
  autocmd FileType cpp,c,h map @i ^i#i <<Esc>A><Esc>
  autocmd FileType cpp,c,h map @I ^i#i "<Esc>A"<Esc>
  autocmd FileType cpp,c,h map @c i/* */<Esc>2hi
  autocmd FileType cpp,c,h map @m iint main(int argc, char* argv[]){}<Esc>Oreturn 0;<Esc>O
  autocmd FileType cpp,c,h map @s iif(){}<Esc>2k3li
  autocmd FileType cpp,c,h map @w iwhile(){}<Esc>2k3li
  autocmd FileType cpp,c,h map @f ifor(){}<Esc>2k3li
augroup END
