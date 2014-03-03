set backspace=indent,eol,start
set backup swapfile updatetime=20000 updatecount=200 undolevels=1000
set encoding=utf-8 fileencoding=utf-8
set tabpagemax=20
set hlsearch incsearch
set listchars=tab:→→,eol:↲,trail:•
set magic
set modeline
set noerrorbells novisualbell
set pastetoggle=<insert>
set relativenumber number
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set tags=./tags,tags;
set ttyfast showcmd hidden
set wildmenu
set laststatus=2
set statusline=%M%h%y\ %t\ %F\ %p%%\ %l/%L\ %=[%{&ff},%{&ft}]\ [a=\%03.3b]\ [h=\%02.2B]\ [%l,%v]

" open files on a new tab
let g:netrw_browse_split=3

" dont load checksyntax, it blinks the screen while saving php files
" let g:checksyntax = -1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['ruby', 'php', 'python', 'c'],
                           \ 'passive_filetypes': [] }

if has("autocmd") && exists("+omnifunc") 
  autocmd Filetype *
    \ if &omnifunc == "" |
    \   setlocal omnifunc=syntaxcomplete#Complete |
    \ endif
endif 

if has('autocmd')
  filetype plugin indent on
endif
