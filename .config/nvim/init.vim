" general settings
set backup
set pastetoggle=<insert>
set relativenumber number
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set tabpagemax=20
set tags=./tags,tags;
set showcmd hidden

" plugins
let s:plugins_base_dir=$HOME . "/.config/nvim/plugins"
let s:dein_dir=s:plugins_base_dir . "/repos/github.com/Shougo/dein.vim"

if !isdirectory(s:dein_dir)
    exe '!git clone https://github.com/Shougo/dein.vim' s:dein_dir
endif

" use the system-wide python and the python-neovim package
let g:python_host_prog='/usr/bin/python2'
let g:python3_host_prog='/usr/bin/python3'
" if !has('python3') && !has('python') && executable('pip')
"   exe '!pip install -U neovim'
" endif

exe "set runtimepath+=" . s:dein_dir

if dein#load_state(s:plugins_base_dir)
  call dein#begin(s:plugins_base_dir)

  call dein#add(s:dein_dir)
  call dein#add('Shougo/denite.nvim')

  call dein#add('Shougo/deoplete.nvim')
  call dein#add('zchee/deoplete-jedi')
  call dein#add('neomake/neomake')

  call dein#add('tpope/vim-commentary')
  call dein#add('tpope/vim-endwise')
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-repeat')
  call dein#add('tpope/vim-surround')

  call dein#add('python.vim')
  call dein#add('editorconfig/editorconfig-vim')

  call dein#end()
  call dein#save_state()
endif

call deoplete#enable()

" mappings
let mapleader = ' '

" move just the content
map <up> <c-y>k
map <down> <c-e>j

" change buffers
map <left> :bnext<cr>
map <right> :bprev<cr>

nnoremap <leader>w :w<cr>
nnoremap <leader>q :q<cr>
nnoremap <leader>n :nohl<cr>
nnoremap <leader>p :set paste!<cr>

nnoremap <leader>c :setlocal <C-R>=<SID>toggle('cursorline')<CR><CR>
nnoremap <leader>u :setlocal <C-R>=<SID>toggle('cursorcolumn')<CR><CR>
nnoremap <leader>l :setlocal <C-R>=<SID>toggle('list')<CR><CR>

" filetype settings
autocmd BufEnter * set completeopt-=preview  " Disable documentation preview

augroup Lisp
  autocmd!
  autocmd FileType lisp set showmatch
augroup END

augroup Vim
  autocmd!
  autocmd FileType vim set shiftwidth=2 softtabstop=2 tabstop=2 expandtab
augroup END

augroup Python
  autocmd!
  autocmd FileType python set nowrap
  autocmd BufWritePre *.py :%s/\s\+$//e
  autocmd BufReadPost,BufWritePost *.py :Neomake
augroup END

augroup C-Files
  autocmd!
  autocmd FileType cpp,c,h set cindent
  autocmd FileType cpp,c,h set cscopetag cscopetagorder=0

if !exists("/usr/share/clang/clang-format.py")
  autocmd FileType cpp,c,h map <leader>f :pyfile /usr/share/clang/clang-format.py<cr>
endif
augroup END
