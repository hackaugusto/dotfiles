let mapleader = ' '

map <up> <c-y>k
map <down> <c-e>j
" map <left> :bnext<cr>
" map <right> :bprev<cr>

nnoremap ]b :bnext<cr>
nnoremap [b :bprevious<cr>
nnoremap ]t :tnext<cr>
nnoremap [t :tprevious<cr>
" nnoremap <silent> <c-]> <c-w><c-]><c-w>T

" Use the original ^w hjkl
" change window
" nnoremap <c-j> <c-w>j
" nnoremap <c-k> <c-w>k
" nnoremap <c-h> <c-w>h
" nnoremap <c-l> <c-w>l

" change tab
nnoremap <m-1> :gt1
nnoremap <m-2> :gt2
nnoremap <m-3> :gt3
nnoremap <m-4> :gt4
nnoremap <m-5> :gt5
nnoremap <m-6> :gt6
nnoremap <m-7> :gt7
nnoremap <m-8> :gt8
nnoremap <m-9> :tablast

" Use Ctrl + C or Ctrl + [ or Alt + <normal mode action>
" quick <esc>
" inoremap jk <esc>
" inoremap JK <esc>
" inoremap Jk <esc>
" nnoremap <f1> <esc>
" inoremap <f1> <esc>

" center the search
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz
nnoremap <silent> <C-o> <C-o>zz
nnoremap <silent> <C-i> <C-i>zz 

" ZZ for write and quit
nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>n :nohl<cr>
nnoremap <leader>p :set paste!<cr>
nnoremap <leader>d :bdelete<cr>

" Open current line on GitHub
noremap <silent> <leader>o :!echo `git url`/blob/`git rev-parse --abbrev-ref HEAD`/%\#L<C-R>=line('.')<CR> \| xargs open<CR><CR>

" file_rec/async requires Shougo/vimproc.vim
nnoremap <silent> <leader>f :<c-u>Unite -no-split -default-action=tabopen -toggle buffer file_mru bookmark file_rec/async:!<cr><c-u>
nnoremap <silent> <leader>t :<c-u>Unite -no-split -default-action=open -quick-match tab buffer<cr>
nnoremap <silent> <leader>h :<c-u>Unite -no-split -default-action=tabopen help<cr>
nnoremap <silent> <leader>i :<c-u>Unite buffer register history/yank mapping<cr>
nnoremap <silent> <leader>/ :<c-u>Unite -no-quit -buffer-name=search grep:.<cr>
nnoremap <silent> <leader>g :<c-u>Unite -silent -quick-match menu:git<CR>

function! LineScrollOtherWindow(dir)
  if a:dir == "down"
    let move = "\<c-e>"
    elseif a:dir == "up"
    let move = "\<c-y>"
  endif
  exec "normal \<c-w>p" . move . "\<c-w>p"
endfun

function! PageScrollOtherWindow(dir)
  if a:dir == "down"
    let move = "\<c-u>"
    elseif a:dir == "up"
    let move = "\<c-d>"
  endif
  exec "normal \<c-w>p" . move . "\<c-w>p"
endfun

nmap <silent> <m-down> :call LineScrollOtherWindow("down")<cr>
nmap <silent> <m-up> :call LineScrollOtherWindow("up")<cr>
nmap <silent> <m-u> :call PageScrollOtherWindow("down")<cr>
nmap <silent> <m-d> :call PageScrollOtherWindow("up")<cr>
nmap <silent> e :call LineScrollOtherWindow("down")<cr>
nmap <silent> y :call LineScrollOtherWindow("up")<cr>
nmap <silent> u :call PageScrollOtherWindow("down")<cr>
nmap <silent> d :call PageScrollOtherWindow("up")<cr>
