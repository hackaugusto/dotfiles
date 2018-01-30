" general settings
set backup
set pastetoggle=<insert>
set relativenumber number
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set tabpagemax=20
set tags=./tags,tags;
set showcmd hidden

" statusline
set laststatus=2

set statusline=%m
" warns for readonly, syntax erros, files not ending in \n and files that are
" not utf8
set statusline+=%#warningmsg#
set statusline+=%{Bracket(Comma(LoclistErrors(),LoclistWarnings(),MixedTabSpace(),TrailingSpace(),Diff(&ff,'unix'),Diff(&fenc,'utf-8'),One(&ro,'RO')))}
set statusline+=%*

set statusline+=\ %t

set statusline+=%{Space(Bracket(Comma(One(&paste,'paste'),Equal(&ff,'unix'),Equal(&fenc,'utf-8'),&ft)))}
set statusline+=%{virtualenv#statusline()}
set statusline+=%{Space(VCSChanges())}%{fugitive#head(7)}

set statusline+=%=
set statusline+=%<

set statusline+=%{StatuslineCurrentHighlight()}
set statusline+=\ [ascii\ %03.3b\ hex\ %02.2B]
set statusline+=\ [col\ %v\ line\ %l/%L\ %p%%]

" required for the pep8 style indentation
filetype plugin indent on

let g:neomake_solidity_solc_maker = {
  \ 'errorformat': '%f:%l:%c:%m',
  \ }
let g:neomake_solidity_enabled_makers = ['solc']

function! Comma(...)
  let items = []
  for item in a:000
    if item != ''
      let items += [item]
    endif
  endfor
  return join(items, ',')
endfunction

function! Space(line)
  return a:line != '' ? ' ' . a:line . ' ' : ''
endfunction

function! Bracket(line)
  return a:line != '' ? '[' . a:line . ']' : ''
endfunction

function! One(attr,text)
  if a:attr == 1
    return a:text
  endif
endfunction

function! Diff(one,other)
  if a:one != a:other
    return a:one
  endif
endfunction

function! Equal(one,other)
  if a:one == a:other
    return a:one
  endif
endfunction

" scrooloose's functions
function! StatuslineCurrentHighlight()
  let name = synIDattr(synID(line('.'),col('.'),1),'name')

  if name != ''
      return '[' . name . ']'
  endif
  return ''
endfunction

function! VCSChanges()
  let hunks = sy#repo#get_stats()
  let parts = []

  if hunks[0] > 0
    call add(parts, get(g:, 'signify_sign_add', '+') . hunks[0])
  endif

  if hunks[1] > 0
    call add(parts, get(g:, 'signify_sign_change', '!') . hunks[1])
  endif

  if hunks[2] > 0
    call add(parts, get(g:, 'signify_sign_delete_first_line', '-') . hunks[2])
  endif

  return join(parts)
endfunction

autocmd cursorhold,bufwritepost * unlet! b:statusline_trailing_space_warning
function! TrailingSpace()
  if !exists("b:statusline_trailing_space_warning")

    if !&modifiable
      let b:statusline_trailing_space_warning = ''
        return b:statusline_trailing_space_warning
    endif

    if search('\s\+$', 'nw') != 0
      let b:statusline_trailing_space_warning = '\s$'
    else
      let b:statusline_trailing_space_warning = ''
    endif
  endif
  return b:statusline_trailing_space_warning
endfunction

autocmd cursorhold,bufwritepost * unlet! b:statusline_tab_warning
function! MixedTabSpace()
  if !exists("b:statusline_tab_warning")
    let b:statusline_tab_warning = ''

    if !&modifiable
      return b:statusline_tab_warning
    endif

    let tabs = search('^\t', 'nw') != 0
    let spaces = search('^ \{' . &ts . ',}[^\t]', 'nw') != 0

    if tabs && spaces
      let b:statusline_tab_warning = 'mixed-indenting'
    elseif (spaces && !&et) || (tabs && &et)
      let b:statusline_tab_warning = '&et'
    endif
  endif
  return b:statusline_tab_warning
endfunction

function! LoclistErrors()
  let loclist = getloclist(0)
  let errors = filter(loclist, "v:val['type'] == 'E'")

  if !empty(errors)
    let num_errors = len(errors)
    return num_errors . 'E@' . errors[0]['lnum']
  endif
endfunction

function! LoclistWarnings()
  let loclist = getloclist(0)
  let warnings = filter(loclist, "v:val['type'] == 'W'")

  if !empty(warnings)
    let num_warnings = len(warnings)
    return num_warnings . 'W@' . warnings[0]['lnum']
  endif
endfunction

" plugins
let s:plugins_base_dir=$HOME . "/.config/nvim/plugins"
let s:dein_dir=s:plugins_base_dir . "/repos/github.com/Shougo/dein.vim"
let s:dein_install=0

if !isdirectory(s:dein_dir)
    exe '!git clone https://github.com/Shougo/dein.vim' s:dein_dir
    let s:dein_install=1
endif

" use the system-wide python and the python-neovim package
let g:python_host_prog='/usr/bin/python2'
let g:python3_host_prog='/usr/bin/python3'

exe "set runtimepath+=" . s:dein_dir
set completeopt+=noinsert

if dein#load_state(s:plugins_base_dir)
  call dein#begin(s:plugins_base_dir)

  call dein#add(s:dein_dir)

  " colorscheme
  call dein#add('nanotech/jellybeans.vim')

  " editing
  call dein#add('tpope/vim-repeat')
  call dein#add('editorconfig/editorconfig-vim')
  call dein#add('pgdouyon/vim-evanesco')  " enhanced search
  call dein#add('SirVer/ultisnips')
  call dein#add('junegunn/vim-easy-align')

  " text objects and operators
  call dein#add('wellle/targets.vim')
  " better (but slower) than surround for unaligned chars
  " call dein#add('machakann/vim-sandwich',
  "   \ {'hook_post_source': 'runtime macros/sandwich/keymap/surround.vim'}
  "   \ )
  " call dein#add('pgdouyon/vim-apparate')
  call dein#add('tpope/vim-surround')

  " presentation
  call dein#add('junegunn/goyo.vim')
  call dein#add('junegunn/limelight.vim')

  " completion
  call dein#add('ctrlpvim/ctrlp.vim')
  call dein#add('mileszs/ack.vim')
  " call dein#add('Valloric/YouCompleteMe') " using AUR's package
  " deoplete
  call dein#add('Shougo/deoplete.nvim')
  call dein#add('eagletmt/neco-ghc')
  call dein#add('racer-rust/vim-racer')
  call dein#add('zchee/deoplete-clang')
  call dein#add('zchee/deoplete-jedi')

  " source control
  call dein#add('mhinz/vim-signify')
  call dein#add('tpope/vim-fugitive')
  call dein#add('junegunn/gv.vim')

  " progamming languages
  call dein#add('sheerun/vim-polyglot')
  call dein#add('tpope/vim-commentary')
  call dein#add('neomake/neomake')
  call dein#add('Shougo/echodoc.vim')
  call dein#add('bhurlow/vim-parinfer')
  call dein#add('tpope/vim-endwise', {'on_ft': [
    \ 'lua', 'elixir', 'ruby', 'crystal', 'sh', 'zsh', 'vb', 'vbnet', 'aspvbs',
    \ 'vim', 'c', 'cpp', 'xdefaults', 'haskell', 'objc', 'matlab', 'htmldjango',
    \ 'snippets'
    \ ]})

  call dein#add('rust-lang/rust.vim')
  call dein#add('eagletmt/neco-ghc')

  " python
  call dein#add('davidhalter/jedi-vim')
  call dein#add('jmcantrell/vim-virtualenv')
  call dein#add('vim-scripts/python_match.vim')
  call dein#add('vim-scripts/python.vim')                 " block motions
  " best indentation for python (installed throught vim-polyglot)
  " call dein#add('mitsuhiko/vim-python-combined')

  call dein#end()
  call dein#save_state()
endif

if s:dein_install
  " :UpdateRemotePlugins
  call dein#install()
endif

colorscheme jellybeans
call deoplete#enable()

let g:deoplete#sources#clang#libclang_path='/usr/lib/libclang.so'
let g:deoplete#sources#clang#clang_header='/usr/lib/clang/5.0.1/include'

let g:ycm_global_ycm_extra_conf = '/usr/share/vim/vimfiles/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'

let g:jedi#completions_enabled = 0
let g:jedi#use_tabs_not_buffers = 1
let g:jedi#goto_command = 'gd'
let g:jedi#goto_assignments_command = 'ga'
let g:jedi#goto_definitions_command = ''
let g:jedi#usages_command = ''
let g:jedi#rename_command = '<leader>r'
let g:jedi#rename_command = ''
let g:jedi#show_call_signatures = 1
let g:jedi#show_call_signatures_delay = 100
let g:jedi#smart_auto_mappings = 0

let g:signify_sign_add = '+'
let g:signify_sign_delete_first_line = '-'
let g:signify_sign_change = '~'

" call dein#add('Shougo/denite.nvim')
" call denite#custom#alias('source', 'file_rec/git', 'file_rec')
" call denite#custom#var('file_rec', 'command', ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
" call denite#custom#var('file_rec/git', 'command', ['git', 'ls-files', '--exclude-standard', '-co'])
" nnoremap <leader>f :<C-u>Denite `finddir('.git', ';') != '' ? 'file_rec/git' : 'file_rec'`<CR>

" call dein#add('wincent/command-t', {'build': 'cd ruby/command-t; make clean; ruby extconf.rb && make'})
" nnoremap <leader>f :CtrlP<cr>
" let g:CommandTFileScanner = 'git'
let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files --exclude-standard -co']
let g:ctrlp_map = ''

let g:ackprg = 'ag --vimgrep'

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

" Up and Down act as ^n and ^p for the autocomplete menu
inoremap <expr><Down> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr><Up> pumvisible() ? "\<C-p>" : "\<Up>"

" https://github.com/neovim/neovim/issues/2068
" inoremap <m-a> <esc>a - same sequence as á
inoremap <m-b> <esc>b
inoremap <m-h> <esc>h
inoremap <m-j> <esc>j
inoremap <m-k> <esc>k
inoremap <m-l> <esc>l
inoremap <m-n> <esc>n
inoremap <m-p> <esc>p
inoremap <m-w> <esc>w
inoremap <m-A> <esc>A
inoremap <m-I> <esc>I

" Tab for autocomplete menu navigation
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : deoplete#mappings#manual_complete()
inoremap <silent><expr> <S-TAB> pumvisible() ? "\<C-p>" : <SID>check_back_space() ? "\<S-TAB>" : deoplete#mappings#manual_complete()

" filetype settings
autocmd BufEnter * set completeopt-=preview  " Disable documentation preview
autocmd VimEnter * call dein#call_hook('post_source')

function! RemoveOldSwap(filename)
  if getftime(v:swapname) < getftime(a:filename)
    let v:swapchoice = 'd'
  endif
endfunction
autocmd SwapExists * call RemoveOldSwap(expand('<afile>:p'))

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

augroup Solidity
  autocmd!
  autocmd BufReadPost,BufWritePost *.rs :Neomake
augroup END

augroup Rust
  autocmd!
  autocmd FileType rust let g:rustfmt_autosave = 1
  autocmd BufWritePre *.rs :%s/\s\+$//e
  autocmd BufReadPost,BufWritePost *.rs :Neomake
augroup END

augroup Haskell
  autocmd!
  autocmd FileType haskell set formatprg=stylish-haskell
  autocmd FileType haskell let g:necoghc_use_stack=1
  autocmd BufWritePre *.hs :%s/\s\+$//e
  autocmd BufReadPost,BufWritePost *.hs :Neomake
augroup END

augroup C-Files
  autocmd!
  autocmd FileType cpp,c,h set cindent
  autocmd FileType cpp,c,h set cscopetag cscopetagorder=0

if !exists("/usr/share/clang/clang-format.py")
  autocmd FileType cpp,c,h map <leader>f :pyfile /usr/share/clang/clang-format.py<cr>
endif
augroup END
