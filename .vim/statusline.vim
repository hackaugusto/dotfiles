set laststatus=2

set statusline=%m
" warns for readonly, syntax erros, files not ending in \n and files that are
" not utf8
set statusline+=%#warningmsg#
set statusline+=%{Bracket(Comma(SyntasticStatuslineFlag(),MixedTabSpace(),TrailingSpace(),Diff(&ff,'unix'),Diff(&fenc,'utf-8'),One(&ro,'RO')))}
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
