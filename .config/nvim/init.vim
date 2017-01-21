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
set statusline+=%{Bracket(Comma(neomake#statusline#LoclistStatus(),MixedTabSpace(),TrailingSpace(),Diff(&ff,'unix'),Diff(&fenc,'utf-8'),One(&ro,'RO')))}
set statusline+=%*

set statusline+=\ %t

set statusline+=%{Space(Bracket(Comma(One(&paste,'paste'),Equal(&ff,'unix'),Equal(&fenc,'utf-8'),&ft)))}
set statusline+=%{virtualenv#statusline()}
set statusline+=%{Space(VCSChanges())}%{fugitive#head(7)}

set statusline+=%=

set statusline+=%{StatuslineCurrentHighlight()}
set statusline+=\ [ascii\ %03.3b\ hex\ %02.2B]
set statusline+=\ [col\ %v\ line\ %l/%L\ %p%%]

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

function! SyntasticErrors()
  let loclist = g:SyntasticLoclist.current()
  let errors = loclist.errors()

  if !empty(errors)
    let num_errors = len(errors)
    return num_errors . 'E@' . errors[0]['lnum']
  endif
endfunction

function! SyntasticWarnings()
  let loclist = g:SyntasticLoclist.current()
  let warnings = loclist.warnings()

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
" if !has('python3') && !has('python') && executable('pip')
"   exe '!pip install -U neovim'
" endif

exe "set runtimepath+=" . s:dein_dir
set completeopt+=noinsert

if dein#load_state(s:plugins_base_dir)
  call dein#begin(s:plugins_base_dir)

  call dein#add(s:dein_dir)
  call dein#add('Shougo/denite.nvim')

  call dein#add('Shougo/deoplete.nvim')
  call dein#add('zchee/deoplete-jedi')
  call dein#add('racer-rust/vim-racer')
  call dein#add('eagletmt/neco-ghc')
  call dein#add('neomake/neomake')
  call dein#add('Shougo/echodoc.vim')

  call dein#add('davidhalter/jedi-vim')

  call dein#add('mhinz/vim-signify')
  call dein#add('jmcantrell/vim-virtualenv')

  call dein#add('tpope/vim-commentary')
  call dein#add('tpope/vim-endwise')
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-repeat')
  call dein#add('tpope/vim-surround')
  call dein#add('tpope/vim-fugitive')

  call dein#add('python.vim')
  call dein#add('editorconfig/editorconfig-vim')

  call dein#add('Shougo/denite.nvim')

  call dein#end()
  call dein#save_state()
endif

if s:dein_install
  call dein#install()
endif

call deoplete#enable()

let g:jedi#completions_enabled = 0

call denite#custom#alias('source', 'file_rec/git', 'file_rec')
call denite#custom#var('file_rec', 'command', ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
call denite#custom#var('file_rec/git', 'command', ['git', 'ls-files', '--exclude-standard', '-co'])

" call dein#add('wincent/command-t', {'build': 'cd ruby/command-t; make clean; ruby extconf.rb && make'})
" call dein#add('ctrlpvim/ctrlp.vim')
" nnoremap <leader>f :CtrlP<cr>
" let g:CommandTFileScanner = 'git'
" let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files --exclude-standard -co']
" let g:ctrlp_map = ''

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
nnoremap <leader>f :<C-u>Denite `finddir('.git', ';') != '' ? 'file_rec/git' : 'file_rec'`<CR>

nnoremap <leader>c :setlocal <C-R>=<SID>toggle('cursorline')<CR><CR>
nnoremap <leader>u :setlocal <C-R>=<SID>toggle('cursorcolumn')<CR><CR>
nnoremap <leader>l :setlocal <C-R>=<SID>toggle('list')<CR><CR>

" Up and Down act as ^n and ^p for the autocomplete menu
inoremap <expr><Down> pumvisible() ? "\<C-n>" : "\<Down>"
inoremap <expr><Up> pumvisible() ? "\<C-p>" : "\<Up>"

" Tab for autocomplete menu navigation
function! s:check_back_space() abort
  let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : <SID>check_back_space() ? "\<TAB>" : deoplete#mappings#manual_complete()
inoremap <silent><expr> <S-TAB> pumvisible() ? "\<C-p>" : <SID>check_back_space() ? "\<S-TAB>" : deoplete#mappings#manual_complete()

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
