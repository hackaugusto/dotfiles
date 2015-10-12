function! g:Cycle()
  let current = winnr()
  let total = winnr('$')
  let next = (current + 1) % (total + 1)
  echomsg "exe " .  next . "wincmd w"
  exe next . "wincmd w"
endfunction

if ! exists('$TMUX')
  tnoremap <C-a>c <C-\><C-n>:tabnew<Enter>:terminal<Enter>
  tnoremap <C-a>n <C-\><C-n>gti
  tnoremap <C-a>p <C-\><C-n>gTi
  tnoremap <C-a>v <C-\><C-n>:vsplit<Enter>:terminal<Enter>
  tnoremap <C-a>h <C-\><C-n>:split<Enter>:terminal<Enter>
  tnoremap <C-a>: <C-\><C-n>:
  tnoremap <C-a>o <C-\><C-n>:call g:Cycle()<Enter>i
  tnoremap <C-a>a <C-\><C-n><C-W><C-P>

  nnoremap <C-a>n gt
  nnoremap <C-a>p gT
endif
