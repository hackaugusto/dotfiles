syntax on                         " syntax must be before highlight

highlight clear TabSel
highlight clear TabFill
highlight clear TabLine
highlight clear TabLineSel
highlight clear TabLineFill
highlight TabLineSel cterm=bold
highlight TabLine ctermfg=Cyan cterm=NONE

if has("gui_running")
  set guifont=Droid\ Sans\ Mono\ 9
  colorscheme gotham
  " set guifont=Inconsolata\ Medium\ 9
  " set guifont=Meslo\ Lg\ S\ 9
  " set guifont=Monaco\ 9
  " set guifont=Source\ Code\ Pro\ 9
  " set guifont=Ubuntu\ Mono\ 9
else
  colorscheme jellybeans            " use this with erosion
  " colorscheme zellner             " use this with erosion
  " colorscheme torte               " use this with solarized
  " set t_Co=256                    " $TERM must contain 256color
  set background=dark
endif
