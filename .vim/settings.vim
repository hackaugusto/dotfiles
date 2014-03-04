set backspace=indent,eol,start
set backup swapfile updatetime=20000 updatecount=200 undolevels=1000
set encoding=utf-8 fileencoding=utf-8
set hlsearch incsearch
set laststatus=2
set listchars=tab:→→,eol:↲,trail:•
set magic
set modeline
set noerrorbells novisualbell
set pastetoggle=<insert>
set relativenumber number
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set tabpagemax=20
set tags=./tags,tags;
set ttyfast showcmd hidden
set wildmenu

" open files on a new tab
let g:netrw_browse_split=3

let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_stl_format = "%t errors (line %F)"
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['ruby', 'php', 'python', 'c'],
                           \ 'passive_filetypes': [] }

let g:startify_files_number = 20

call neocomplete#initialize()
let g:ycm_filetype_whitelist = {'noop': 1}


if has("autocmd") && exists("+omnifunc")
  autocmd Filetype *
    \ if &omnifunc == "" |
    \   setlocal omnifunc=syntaxcomplete#Complete |
    \ endif
endif

if has('autocmd')
  filetype plugin indent on
endif

if executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts='--no-heading --no-color -a -C4'
  let g:unite_source_grep_recursive_opt=''
endif

if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nocolor --nogroup -S -C4'
  let g:unite_source_grep_recursive_opt=''
endif

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#set_profile('files', 'smartcase', 1)
call unite#custom#source('line,outline','matchers','matcher_fuzzy')

nnoremap <silent> [unite]<space> :<C-u>Unite -toggle -auto-resize -buffer-name=mixed file_rec/async:! buffer file_mru bookmark<cr><c-u>
nnoremap <silent> [unite]f :<C-u>Unite -toggle -auto-resize -buffer-name=files file_rec/async:!<cr><c-u>
nnoremap <silent> [unite]e :<C-u>Unite -buffer-name=recent file_mru<cr>
nnoremap <silent> [unite]y :<C-u>Unite -buffer-name=yanks history/yank<cr>
nnoremap <silent> [unite]l :<C-u>Unite -auto-resize -buffer-name=line line<cr>
nnoremap <silent> [unite]b :<C-u>Unite -auto-resize -buffer-name=buffers buffer<cr>
nnoremap <silent> [unite]/ :<C-u>Unite -no-quit -buffer-name=search grep:.<cr>
nnoremap <silent> [unite]m :<C-u>Unite -auto-resize -buffer-name=mappings mapping<cr>
nnoremap <silent> [unite]s :<C-u>Unite -quick-match buffer<cr>
