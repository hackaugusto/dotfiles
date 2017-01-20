function ConfigureSyntastic(type)
  if a:type == 'python' && has('python')
    let builtins = system("python2 -c 'print(\",\".join(dir(__builtins__) + [\"__name__\", \"__file__\", \"__loader__\", \"__package__\", \"__main__\"]))'")
    let g:syntastic_python_flake8_post_args = " --builtins=".builtins
  endif
endfunction

function FindDjangoSettings()
  if strlen($VIRTUAL_ENV) && has('python')
    let output  = system("find $VIRTUAL_ENV '(' -name 'settings' -type d ')' -or '(' -name 'settings.py' -not -wholename '*site-packages*' ')' | tr '\n' ' '")
    let outarray= split(output, '[\/]\+')

    if len(outarray)
      let module  = outarray[-2] . '.' . 'settings'
      let syspath = system("python -c 'import sys; print(sys.path)' | tr '\n' ' ' ")

      execute 'python import sys, os'
      execute 'python sys.path = ' . syspath
      execute 'python os.environ.setdefault("DJANGO_SETTINGS_MODULE", "' . module . '")'
    endif
  endif
endfunction

function s:source_if_exists(path)
  if filereadable(a:path)
    exec "source " . a:path
  endif
endfunction

if !exists("autocmd_latex")
  autocmd BufNewFile,BufRead tex let autocmd_latex=1
  autocmd BufNewFile,BufRead tex setlocal textwidth=80
  autocmd BufNewFile,BufRead tex let g:tex_flavor = "latex"
endif

" highlight from the beginning of the file
autocmd BufEnter * :syntax sync fromstart

" Disable documentation preview
autocmd BufEnter * set completeopt-=preview

" Asymptote
autocmd BufNewFile,BufRead *.asy setfiletype asy
autocmd Syntax asy call s:source_if_exists("/usr/share/texmf-dist/asymptote/asy.vim")

augroup Lisp
  autocmd!
  autocmd FileType lisp set showmatch
augroup END

augroup Css
  autocmd!
  autocmd FileType css set omnifunc=csscomplete#CompleteCSS
augroup END

" augroup SH
"   autocmd!
"   autocmd FileType sh set number
" augroup END

augroup Vim
  autocmd!
  autocmd FileType vim set shiftwidth=2 softtabstop=2 tabstop=2 
augroup END

augroup JAVASCRIPT
  autocmd!
  " autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType javascript set cindent
  autocmd FileType javascript let g:JSLintHighlightErrorLine = 0
augroup END

augroup CSHARP
  autocmd!
  autocmd FileType cs set cindent
augroup END

augroup LATEX
  autocmd!
  autocmd FileType tex let g:tex_comment_nospell=1
  autocmd FileType tex set spell
  autocmd FileType tex syn match texComment /%.*$/ contains=@texCommentGroup,@NoSpell
augroup END

augroup XML
  autocmd!
  autocmd FileType xml set shiftwidth=2 softtabstop=2 tabstop=2 nocindent noautoindent
augroup END

augroup JADE
  autocmd!
  autocmd FileType jade set shiftwidth=2 softtabstop=2 tabstop=2 nocindent noautoindent
augroup END

augroup Cobra
  autocmd!
  autocmd FileType cobra set nocindent noautoindent
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
  autocmd FileType xhtml,html set omnifunc=htmlcomplete#CompleteTags
  autocmd FileType xhtml,html set shiftwidth=2 softtabstop=2 tabstop=2
augroup END

augroup PHP
  autocmd!
  autocmd FileType php set cindent
  autocmd FileType php let php_sql_query = 1
  autocmd FileType php let php_htmlInStrings = 1
  autocmd FileType php let php_smart_members = 1
augroup END

augroup Java
  autocmd!
  autocmd FileType java set cindent 
  autocmd FileType java set complete=.,i,d omnifunc=javacomplete#Complete
  autocmd FileType java let g:SuperTabDefaultCompletionType='context'
  autocmd FileType java set shiftwidth=4 softtabstop=4 tabstop=4
augroup END

augroup Python
  autocmd!
  autocmd FileType python abbreviate #i import
  autocmd FileType python map @i ^i#i
  autocmd FileType python set omnifunc=pythoncomplete#Complete completeopt-=preview
  autocmd FileType python set nowrap shiftwidth=4 softtabstop=4
  autocmd FileType python let g:SuperTabDefaultCompletionType='context'
  autocmd FileType python let python_highlight_all=1
  autocmd FileType python let g:tags_global_tags = {'py/stdlib': '/usr/lib/python2.7'}
  " disable jedi doc-preview
  autocmd FileType python setlocal completeopt-=preview
  autocmd FileType python call FindDjangoSettings()
  " autocmd FileType python let g:flake8_builtins="_,apply"
  " autocmd FileType python let ropevim_vim_completion=1
  " autocmd FileType python let ropevim_extended_complete = 1
  " autocmd FileType python let g:ropevim_autoimport_modules = ["os.*", "django.*"]
  " autocmd FileType python imap <c-space> <C-R>=RopeCodeAssistInsertMode()<CR>
  autocmd BufReadPost *.py call ConfigureSyntastic('python')
  autocmd BufWritePre *.py :%s/\s\+$//e
  " autocmd BufWritePre *.py :%s/\s\+$//e|''
  " autocmd BufWritePost *.py call Flake8()
augroup END

augroup Perl
  autocmd!
  autocmd FileType perl set autoindent smartindent cindent
  autocmd FileType perl set shiftwidth=4 softtabstop=4 tabstop=4
  autocmd FileType perl set makeprg=perl\ -c\ %\ $* errorformat=%f:%l:%m
  autocmd FileType perl let perl_include_pod=1
  autocmd FileType perl let perl_extended_vars=1
augroup END

augroup C-Files
  autocmd!
  autocmd FileType cpp,c,h set cindent
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

  " use the configuration files
  "   - .syntastic_c_config
  "   - .syntastic_cpp_config
  "   - .syntastic_clang_check_config
  "   - .syntastic_clang_tidy_config
  autocmd FileType c,h let g:syntastic_c_compiler_options='-std=gnu++11'
  autocmd FileType cpp,h let g:syntastic_cpp_compiler_options='-std=gnu++11'

if !exists("/usr/share/clang/clang-format.py")
  autocmd FileType cpp,c,h map <leader>f :pyfile /usr/share/clang/clang-format.py<cr>
endif
augroup END

augroup RUST
  autocmd FileType rust let g:racer_cmd = "/usr/bin/racer"
  autocmd FileType rust let $RUST_SRC_PATH = "/usr/src/rust/src/"
augroup END
