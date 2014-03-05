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

nnoremap <f1> <esc>
inoremap <f1> <esc>
