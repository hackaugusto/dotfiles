" Vim color file
" Maintainer:   Ramon Rocha <ramon.rocha@live.com>
" Last Change:  02 Jul 2012
" Remark:       Designed to match code sample at http://cobra-language.com

set background=dark
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name = "cobra"

" The xterm-color-table script located at the following URL is really useful
" for seeing which cterm values correspond to what colors.
" http://www.vim.org/scripts/script.php?script_id=3412

hi Normal         ctermfg=White   ctermbg=NONE    cterm=NONE  guifg=White   guibg=Black  gui=NONE
hi LineNr         ctermfg=White   ctermbg=233     cterm=NONE  guifg=White   guibg=#111   gui=NONE

hi Comment        ctermfg=62      ctermbg=NONE    cterm=NONE  guifg=#559    guibg=NONE   gui=NONE
hi SpecialComment ctermfg=33      ctermbg=NONE    cterm=NONE  guifg=#37D    guibg=NONE   gui=NONE
hi Todo           ctermfg=Black   ctermbg=Yellow  cterm=bold  guifg=Black   guibg=Yellow gui=bold

hi Constant       ctermfg=77      ctermbg=NONE    cterm=NONE  guifg=#A3D39C guibg=NONE   gui=NONE
hi Character      ctermfg=77      ctermbg=NONE    cterm=NONE  guifg=#A3D39C guibg=NONE   gui=NONE
hi String         ctermfg=77      ctermbg=NONE    cterm=NONE  guifg=#A3D39C guibg=NONE   gui=NONE
hi Number         ctermfg=77      ctermbg=NONE    cterm=NONE  guifg=#A3D39C guibg=NONE   gui=NONE
hi Float          ctermfg=77      ctermbg=NONE    cterm=NONE  guifg=#A3D39C guibg=NONE   gui=NONE
hi Boolean        ctermfg=77      ctermbg=NONE    cterm=NONE  guifg=#A3D39C guibg=NONE   gui=NONE

hi Identifier     ctermfg=White   ctermbg=NONE    cterm=bold  guifg=White   guibg=NONE   gui=bold
hi Function       ctermfg=White   ctermbg=NONE    cterm=bold  guifg=White   guibg=NONE   gui=bold

hi Statement      ctermfg=184     ctermbg=NONE    cterm=NONE  guifg=#FFF000 guibg=NONE   gui=NONE
hi Conditional    ctermfg=184     ctermbg=NONE    cterm=NONE  guifg=#FFF000 guibg=NONE   gui=NONE
hi Repeat         ctermfg=184     ctermbg=NONE    cterm=NONE  guifg=#FFF000 guibg=NONE   gui=NONE
hi Label          ctermfg=184     ctermbg=NONE    cterm=NONE  guifg=#FFF000 guibg=NONE   gui=NONE
hi Operator       ctermfg=184     ctermbg=NONE    cterm=NONE  guifg=#FFF000 guibg=NONE   gui=NONE
hi Keyword        ctermfg=184     ctermbg=NONE    cterm=NONE  guifg=#FFF000 guibg=NONE   gui=NONE
hi Exception      ctermfg=184     ctermbg=NONE    cterm=NONE  guifg=#FFF000 guibg=NONE   gui=NONE

hi PreProc        ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi Include        ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi Define         ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi Macro          ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi PreCondit      ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE

hi Type           ctermfg=196     ctermbg=NONE    cterm=NONE  guifg=Red     guibg=NONE   gui=NONE
hi StorageClass   ctermfg=196     ctermbg=NONE    cterm=NONE  guifg=Red     guibg=NONE   gui=NONE
hi Structure      ctermfg=45      ctermbg=NONE    cterm=NONE  guifg=#6DCFF6 guibg=NONE   gui=NONE
hi Typedef        ctermfg=45      ctermbg=NONE    cterm=NONE  guifg=#6DCFF6 guibg=NONE   gui=NONE

hi Special        ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi SpecialChar    ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi Tag            ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi Delimiter      ctermfg=129     ctermbg=NONE    cterm=NONE  guifg=#A0A    guibg=NONE   gui=NONE
hi Debug          ctermfg=208     ctermbg=NONE    cterm=NONE  guifg=#FFA200 guibg=NONE  gui=NONE
