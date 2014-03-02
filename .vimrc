" Some plugins might be CRLF ended, be nice and use LF only:
"
" find ~/.vim -type f -not -path '*/.git/*' -print0
"   | xargs -0 file -N
"   | grep CRLF
"   | cut -d ':' -f 1
"   | parallel --files cat {} \| tr -d '\\r' \| sponge {}

set nocompatible
filetype off

" https://github.com/gmarik/vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle' 

Bundle 'cpp.vim'
Bundle 'php.vim'
Bundle 'smarty.vim'
Bundle 'python.vim'
Bundle 'django.vim'
Bundle "ekalinin/Dockerfile.vim"
" Bundle 'haskell.vim'
" Bundle 'digitaltoad/vim-jade'
Bundle 'groenewege/vim-less'
Bundle 'jnwhiteh/vim-golang'
Bundle 'kchmck/vim-coffee-script'
Bundle 'leshill/vim-json'
Bundle 'mutewinter/nginx.vim'
Bundle 'mutewinter/vim-css3-syntax'
Bundle 'mutewinter/vim-tmux'
Bundle 'nono/vim-handlebars'
Bundle 'othree/html5.vim'
Bundle 'spf13/vim-gocode'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-markdown'
Bundle 'tpope/vim-fugitive'
" Bundle 'vim-scripts/javascript.vim'
" fixed windows line ending
Bundle 'hackaugusto/javascript.vim'   
Bundle 'wavded/vim-stylus'
Bundle 'file:///mnt/extra/code/rust.vim'

Bundle 'ack.vim'
" Bundle 'ag.vim'
" Bundle 'man.vim'
Bundle 'Align'
Bundle 'HTML-AutoCloseTag'
Bundle 'indenthtml.vim'
Bundle 'matchit.zip'
Bundle 'netrw.vim'
Bundle 'prettyprint.vim'
Bundle 'python_match.vim'
Bundle 'SQLUtilities'

Bundle 'tpope/vim-surround'
" Bundle 'justinmk/vim-sneak'         " / with incsearch works just as fine
Bundle 'bogado/file-line'
"Bundle 'paradigm/vim-multicursor'
Bundle 'terryma/vim-multiple-cursors'
Bundle 'tpope/vim-dispatch'
" Bundle 'szw/vim-tags'               
" added upwards search for `g:vim_tags_directories`
Bundle 'hackaugusto/vim-tags'         
" Bundle 'Yggdroot/indentLine'

" Bundle 'hallettj/jslint.vim'
" Bundle 'fly.vim'
" Bundle 'checksyntax'                " Replaced by syntastic
Bundle 'scrooloose/syntastic' 

" Bundle 'altercation/vim-colors-solarized'

Bundle 'javacomplete'
" Bundle 'OmniCppComplete'
" Bundle 'pythoncomplete'
" Bundle 'dbext.vim'                  " Required by SQLComplete
" Bundle 'SQLComplete.vim'            " Tries to complete after every dot, very annoying (probably because of YouCompleteMe)
" omnicomplete based on syntax highlight
Bundle 'vim-scripts/SyntaxComplete'   
" Bundle 'SuperTab'                   " Replaced by YouCompleteMe
" Bundle 'Valloric/YouCompleteMe'     " (needs compilation) requires clang and jedi. installed the AUR package
" Bundle 'davidhalter/jedi'           " YouCompleteMe comes with jedi
" Bundle 'SearchComplete'             " /<Up> not working

if has("autocmd") && exists("+omnifunc") 
  autocmd Filetype *
    \ if &omnifunc == "" |
    \   setlocal omnifunc=syntaxcomplete#Complete |
    \ endif
endif 

if has('autocmd')
  filetype plugin indent on
endif

syntax on                             " syntax must be before highlight
" colorscheme torte                   " use this with solarized
colorschem zellner                    " use this with erosion

"set t_Co=16
"set t_Co=256
"set background=dark
"let g:solarized_termcolors=16

highlight clear TabSel 
highlight clear TabFill
highlight clear TabLine 
highlight clear TabLineSel
highlight clear TabLineFill
highlight clear StatusLine
highlight clear StatusLineNC
highlight TabLineSel cterm=bold
highlight TabLine ctermfg=Cyan cterm=NONE

set backspace=indent,eol,start
set backup swapfile updatetime=20000 updatecount=200 undolevels=1000
set encoding=utf-8 fileencoding=utf-8
set tabpagemax=20
set hlsearch incsearch
set listchars=tab:→→,eol:↲,trail:•
set magic
set modeline
set nocompatible
set noerrorbells novisualbell
set pastetoggle=<insert>
set relativenumber number
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set tags=./tags,tags;                 " :help file-searching
set ttyfast showcmd hidden
set wildmenu
set laststatus=2
set statusline=%M%h%y\ %t\ %F\ %p%%\ %l/%L\ %=[%{&ff},%{&ft}]\ [a=\%03.3b]\ [h=\%02.2B]\ [%l,%v]

let g:netrw_browse_split=3            " open files on a new tab
" let g:checksyntax = -1 " dont load checksyntax, it blinks the screen while saving php files
" let g:syntastic_debug = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['ruby', 'php', 'python', 'c'],
                           \ 'passive_filetypes': [] }

nmap <silent> <m-down> :call LineScrollOtherWindow("down")<CR>
nmap <silent> <m-up> :call LineScrollOtherWindow("up")<CR>
nmap <silent> <m-u> :call PageScrollOtherWindow("down")<CR>
nmap <silent> <m-d> :call PageScrollOtherWindow("up")<CR>
nmap <silent> e :call LineScrollOtherWindow("down")<CR>
nmap <silent> y :call LineScrollOtherWindow("up")<CR>
nmap <silent> u :call PageScrollOtherWindow("down")<CR>
nmap <silent> d :call PageScrollOtherWindow("up")<CR>

map <Up> k
map <Down> j
map <Left> :bnext<CR>
map <Right> :bprev<CR>

nnoremap ]b :bnext<cr>
nnoremap [b :bprevious<cr>
nnoremap ]t :tnext<cr>
nnoremap [t :tprevious<cr>

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

nnoremap <silent><C-]> <C-w><C-]><C-w>T

inoremap jk <esc>
inoremap JK <esc>
inoremap Jk <esc>

" https://github.com/paradigm/dotfiles/blob/master/.vimrc
" use ZZ for write and quit
nnoremap <space>w :w<cr>
nnoremap <space>q :q<cr>

nnoremap <space>n :nohl<cr>
nnoremap <space>p :set paste!<cr>

" nnoremap <c-s> :w

nnoremap <f1> <esc>
inoremap <f1> <esc>

" TODO: not a vim problem, but related
"   I want to use the `[ and `] marks, but in with my current X.org settings
"   I need to  type the ` twice while shift is holded, figure out at least
"   how can I change X's the keyboard configuration to return `[ with a single `
"   keystroke

if !exists("autocmd_latex")
  autocmd BufNewFile,BufRead tex let autocmd_latex=1
  autocmd BufNewFile,BufRead tex setlocal textwidth=80
  autocmd BufNewFile,BufRead tex let g:tex_flavor = "latex"
endif

function! LineScrollOtherWindow(dir)
  if a:dir == "down"
    let move = "\<C-E>"
    elseif a:dir == "up"
    let move = "\<C-Y>"
  endif
  exec "normal \<C-W>p" . move . "\<C-W>p"
endfun

function! PageScrollOtherWindow(dir)
  if a:dir == "down"
    let move = "\<C-U>"
    elseif a:dir == "up"
    let move = "\<C-D>"
  endif
  exec "normal \<C-W>p" . move . "\<C-W>p"
endfun

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

    execute 'python import sys, os'
    execute 'python sys.path = ' . syspath
    execute 'python os.environ.setdefault("DJANGO_SETTINGS_MODULE", "' . module . '")'
  endif
endfunction

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

augroup LATEX
  autocmd!
  autocmd FileType tex let g:tex_comment_nospell=1
  autocmd FileType tex syn match texComment /%.*$/ contains=@texCommentGroup,@NoSpell
augroup END

augroup HASKELL
  autocmd!
  autocmd FileType haskell set shiftwidth=4 softtabstop=4 tabstop=4
augroup END

augroup XML
  autocmd!
  autocmd FileType xml set shiftwidth=2 softtabstop=2 tabstop=2 expandtab nocindent noautoindent
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
augroup END

augroup Python
  autocmd!
  autocmd FileType python abbreviate #i import
  autocmd FileType python map @i ^i#i
  autocmd FileType python set omnifunc=pythoncomplete#Complete completeopt-=preview
  autocmd FileType python set expandtab nowrap shiftwidth=4 softtabstop=4
  autocmd FileType python let g:SuperTabDefaultCompletionType='context'
  autocmd FileType python let python_highlight_all=1
  autocmd FileType python let g:tags_global_tags = {'py/stdlib': '/usr/lib/python2.7'}
  " autocmd FileType python let g:flake8_builtins="_,apply"
  autocmd FileType python call FindDjangoSettings()
  " autocmd BufWritePre *.py :%s/\s\+$//e|''
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
