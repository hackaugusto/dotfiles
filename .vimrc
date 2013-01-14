" General stuff
set shiftwidth=4 softtabstop=4 tabstop=4 expandtab
set backup swapfile updatetime=20000 updatecount=200
" allow to backscape over anything within insert mode
set backspace=indent,eol,start
set noerrorbells novisualbell
set ttyfast showcmd hidden
set hlsearch
set wildmenu
set modeline
set nocompatible
set encoding=utf-8

" netrw
let g:netrw_browse_split=3 " open new files one a new tab

let g:checksyntax = -1 " dont load checksyntax, it blinks the screen while saving php files
let g:syntastic_enable_signs = 1 

highlight clear TabSel 
highlight clear TabFill
highlight clear TabLine 
highlight clear TabLineSel
highlight clear TabLineFill
highlight TabLineSel cterm=bold
highlight TabLine ctermfg=Cyan cterm=NONE
highlight clear StatusLine
highlight clear StatusLineNC

" set t_Co=16
set t_Co=256
set background=dark
let g:solarized_termcolors=16
colorscheme solarized

"set statusline=\ \%f%m%r%h%w\ ::\ %y\ [%{&ff}]\%=\ [%p%%:\ %l/%L]\ 

syntax on
if has('autocmd')
	filetype on
  filetype plugin on
  filetype indent on
endif

runtime ftplugin/man.vim

" jk is faster to exist insert mode 
inoremap jk <esc>

if !exists("autocmd_latex")
  autocmd BufNewFile,BufRead tex let autocmd_latex=1
  autocmd BufNewFile,BufRead tex setlocal textwidth=80
  autocmd BufNewFile,BufRead tex let g:tex_flavor = "latex"
endif

augroup Lisp
  autocmd!
  autocmd FileType lisp set number showmatch
augroup END

augroup SH
  autocmd!
  autocmd FileType sh set number
augroup END

augroup Vim
  autocmd!
  autocmd FileType vim set shiftwidth=2 softtabstop=2 tabstop=2 expandtab
	autocmd FileType vim set number
augroup END

augroup JAVASCRIPT
	autocmd!
	autocmd FileType javascript set shiftwidth=4 softtabstop=4 tabstop=4
	autocmd FileType javascript set number cindent
augroup END

augroup CSHARP
	autocmd!
	autocmd FileType cs set shiftwidth=4 softtabstop=4 tabstop=4
	autocmd FileType cs set number cindent
augroup END

augroup HASKELL
	autocmd!
	autocmd FileType haskell set shiftwidth=4 softtabstop=4 tabstop=4
	autocmd FileType haskell set number
augroup END

augroup XML
	autocmd!
  autocmd FileType xml set shiftwidth=2 softtabstop=2 tabstop=2 expandtab nocindent noautoindent
  "autocmd FileType xml source ~/.vim/ftplugin/html_autoclosetag.vim
  "autocmd FileType xhtml,html source ~/.vim/ftplugin/closetag.vim
augroup END

augroup JADE
	autocmd!
  autocmd FileType jade set shiftwidth=2 softtabstop=2 tabstop=2 expandtab nocindent noautoindent
augroup END

augroup Cobra
	autocmd!
  autocmd FileType cobra set shiftwidth=4 softtabstop=4 tabstop=4 expandtab nocindent noautoindent
	autocmd FileType cobra set number
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

"	autocmd FileType xhtml,html so ~/.vim/ftplugin/html_autoclosetag.vim
"	autocmd FileType xhtml,html so ~/.vim/ftplugin/closetag.vim
augroup END

augroup PHP
  autocmd!
  autocmd FileType php set cindent number
	autocmd FileType php let php_sql_query = 1
	autocmd FileType php let php_htmlInStrings = 1
	autocmd FileType php let php_smart_members = 1
augroup END

augroup Java
  autocmd!
  autocmd FileType java set cindent number expandtab
  autocmd FileType java set complete=.,i,d omnifunc=javacomplete#Complete 
  autocmd FileType java let g:SuperTabDefaultCompletionType='context'
  autocmd FileType java set shiftwidth=4 softtabstop=4 tabstop=4 

	"autocmd BufRead *.java set efm=%A\ %#[javac]\ %f:%l:\ %m,%-Z\ %#[javac]\ %p^,%-C%.%#
	"autocmd BufRead set makeprg=ant\ -find\ build.xml
augroup END

augroup Python
  autocmd!
  autocmd FileType python abbreviate #i import
  autocmd FileType python map @i ^i#i
  autocmd FileType python set number omnifunc=pythoncomplete#Complete completeopt-=preview
  autocmd FileType python let g:SuperTabDefaultCompletionType='context'
  autocmd FileType python set expandtab nowrap shiftwidth=4 softtabstop=4
  autocmd FileType python let python_highlight_all=1
	autocmd BufWritePre *.py :%s/\s\+$//e
augroup END

augroup Perl
	autocmd!
	autocmd FileType perl set autoindent smartindent number cindent
	autocmd FileType perl set shiftwidth=4 softtabstop=4 tabstop=4
	autocmd FileType perl set makeprg=perl\ -c\ %\ $* errorformat=%f:%l:%m
	autocmd FileType perl let perl_include_pod=1
	autocmd FileType perl let perl_extended_vars=1
augroup END

augroup C-Files
  autocmd!
  autocmd FileType cpp,c,h set cindent number
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
