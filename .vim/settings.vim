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

let g:netrw_browse_split = 3                " open files on a new tab
let g:neomru#file_mru_limit = 20            " use with -no-split
let g:startify_files_number = 20
let g:startify_skiplist = ['\~$']
let g:ycm_filetype_whitelist = {'noop': 1}  " diable for everything while testing neocomplete
let g:signify_sign_add = '+'
let g:signify_sign_delete_first_line = '-'
let g:signify_sign_change = '~'
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_stl_format = "%t errors (line %F)"
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['ruby', 'php', 'python', 'c'],
                           \ 'passive_filetypes': [] }
let g:unite_source_menu_menus = {}
let g:unite_source_menu_menus.git = {'description' : 'git'}
let g:unite_source_menu_menus.git.command_candidates = [
    \['▷ tig', 'Unite tig -no-split'],
    \['▷ git status', 'Gstatus'],
    \['▷ git diff', 'Gdiff'],
    \['▷ git commit', 'Gcommit'],
    \['▷ git log', 'exe "silent Glog | Unite quickfix"'],
    \['▷ git blame', 'Gblame'],
    \['▷ git stage', 'Gwrite'],
    \['▷ git checkout', 'Gread'],
    \['▷ git rm', 'Gremove'],
    \['▷ git mv', 'exe "Gmove " input("directory: ")'],
    \['▷ git push', 'Git! push'],
    \['▷ git pull', 'Git! pull'],
    \['▷ git prompt', 'exe "Git! " input("git ")'],
    \['▷ git cd', 'Gcd'],
    \]

call neocomplete#initialize()
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#set_profile('files', 'smartcase', 1)
call unite#custom#source('line,outline','matchers','matcher_fuzzy')

if has("autocmd") && exists("+omnifunc")
  autocmd Filetype *
    \ if &omnifunc == "" |
    \   setlocal omnifunc=syntaxcomplete#Complete |
    \ endif
endif

if has('autocmd')
  filetype plugin indent on
endif

if executable('ag')
  set grepprg=ag\ --nogroup\ --noheading\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nogroup --noheading --column --smart-case --nocolor --follow -C0'
  let g:unite_source_grep_recursive_opt=''
elseif executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
  let g:unite_source_grep_command='ack'
  let g:unite_source_grep_default_opts='--no-heading --no-color -a -C4'
  let g:unite_source_grep_recursive_opt=''
endif
