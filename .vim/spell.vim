" A list of directories used to save the spell file.
if !exists('g:spell_directories')
    let g:spell_directories = ['.git', '.svn', 'CVS', '.hg']
endif

if !exists('g:spell_file')
    let g:spell_file = 'spell'
endif

function! s:spell()
  for directory in g:spell_directories
    let directory = simplify(finddir(directory, ';'))  " Search upwards
    
    if !empty(directory)
      let spell_file = simplify(directory . '/' .  g:spell_file . '.utf8.add')
      silent! exe 'setlocal spellfile+=' . spell_file
      break
    endif
  endfor
endfunction

autocmd BufEnter * :call s:spell()
