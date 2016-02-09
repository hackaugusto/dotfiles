set backspace=indent,eol,start
set backup swapfile updatetime=20000 updatecount=200 undolevels=1000
set encoding=utf-8 fileencoding=utf-8
set guioptions=
set hlsearch incsearch
set laststatus=2
set listchars=tab:>-,eol:↲,trail:•
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

function! s:highlight_mixed_spaces()
  " dont bother highlighting system files
  if stridx(@%, "/usr") != 0
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$\| \+\ze\t\+\|[^\t]\zs\t\+/
  endif
endfunction

autocmd BufReadPost * call s:highlight_mixed_spaces()

let g:limelight_conceal_ctermfg = 'gray'

let g:netrw_browse_split = 3                            " open files on a new tab
let g:netrw_list_hide = '[.]pyc$,[.]pyo$,[~]$,[.]swp$'  " hide python objects and vim backups

let g:startify_files_number = 20
let g:startify_skiplist = ['\~$']

let g:signify_sign_add = '+'
let g:signify_sign_delete_first_line = '-'
let g:signify_sign_change = '~'

" Using custom <tab> mapping to handle the menu (it's defined on mappings.vim)
let g:UltiSnipsExpandTrigger = '<Plugin>'         " UltiSnips has no settings to disable automatic mappings, so create one that cannot be used
let g:ycm_key_list_select_completion = []
let g:ycm_key_list_previous_completion = []

" These mappings are added when a snippet is found and restored afterwards, so
" there is not a compatibility problem.
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

" stoped using neocomplete in favor of YouCompleteMe
" call neocomplete#initialize()
"let g:ycm_filetype_whitelist = {'noop': 1}       " disable for everything while testing neocomplete

let g:ycm_key_detailed_diagnostics = ''           " using <leader>d from jedi
let g:ycm_key_invoke_completion = ''              " using completion on two characters and on semantic characters
let g:ycm_complete_in_comments = 0
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_add_preview_to_completeopt = 0          " disable docs
let g:ycm_server_keep_logfiles = 0
let g:ycm_server_log_level = 'info'
let g:ycm_filetype_specific_completion_to_disable = {
  \ 'java': 1
  \}
let g:ycm_semantic_triggers =  {
  \ 'rust': ['.', '::']
  \}
" right now ycm is using too much memory (about 4G), and it's unusable
let g:ycm_filetype_blacklist = {
  \ 'python' : 1,
  \}

" ycm compatibility
let g:EclimCompletionMethod = 'omnifunc'

" using jedi for the goto feature
let g:jedi#usages_command = '<leader>u'           " <leader>n is mapped to :nohl
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0             " don't want completeopt=longest,preview neither inoremap <C-c> <ESC>
let g:jedi#goto_command = 'gd'
let g:jedi#popup_on_dot = 1
let g:jedi#popup_select_first = 1
let g:jedi#show_call_signatures = 1
let g:jedi#use_tabs_not_buffers = 1
let g:jedi#smart_auto_mappings = 0

let g:syntastic_check_on_open = 1
let g:syntastic_aggregate_errors = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_stl_format = "%t errors (line %F)"
let g:syntastic_mode_map = {
  \'mode': 'active',
  \'active_filetypes': ['ruby', 'php', 'python', 'c'],
  \'passive_filetypes': ['tex'] }
" prefer the clang's compilation database
let g:syntastic_cpp_clang_check_post_args = ""
let g:syntastic_cpp_clang_tidy_post_args = ""

let g:neomru#file_mru_limit = 20            " use with -no-split

let g:unite_source_menu_menus = {}
let g:unite_source_menu_menus.git = {'description' : 'git'}
let g:unite_source_menu_menus.git.command_candidates = [
  \['tig', 'Unite tig -no-split'],
  \['git status', 'Gstatus'],
  \['git diff', 'Gdiff'],
  \['git commit', 'Gcommit'],
  \['git log', 'exe "silent Glog | Unite quickfix"'],
  \['git blame', 'Gblame'],
  \['git stage', 'Gwrite'],
  \['git checkout', 'Gread'],
  \['git rm', 'Gremove'],
  \['git mv', 'exe "Gmove " input("directory: ")'],
  \['git push', 'Git! push'],
  \['git pull', 'Git! pull'],
  \['git prompt', 'exe "Git! " input("git ")'],
  \['git cd', 'Gcd'],
  \]

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
" call unite#set_profile('files', 'smartcase', 1)
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

let g:unite_source_grep_recursive_opt = ''

if executable('ag')
  set grepformat=%f:%l:%c:%m
  set grepprg=ag\ --nogroup\ --noheading\ --column\ --smart-case\ --nocolor\ --follow\ --nobreak
  let g:ackprg="ag\\ --nogroup\\ --noheading\\ --column\\ --smart-case\\ --nocolor\\ --follow\\ --nobreak"
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts =
    \ '-i --vimgrep --hidden --ignore ' .
    \ '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
elseif executable('pt')
  let g:unite_source_grep_command = 'pt'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor'
elseif executable('ack')
  set grepformat=%f:%l:%c:%m
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  let g:unite_source_grep_command = 'ack'
  let g:unite_source_grep_default_opts =
    \ '-i --no-heading --no-color -k -H'
elseif executable('ack-grep')
  set grepformat=%f:%l:%c:%m
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  let g:unite_source_grep_command = 'ack-grep'
  let g:unite_source_grep_default_opts =
    \ '-i --no-heading --no-color -k -H'
endif
