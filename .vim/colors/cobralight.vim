" Vim color file
" Maintainer:   Ramon Rocha <ramon.rocha@live.com>
" Last Change:  02 Jul 2012
" Remark:       Based on http://cobra-language.com/how-to/WriteBasicSyntax

set background=light
hi clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name = "cobralight"

" The xterm-color-table script located at the following URL is really useful
" for seeing which cterm values correspond to what colors.
" http://www.vim.org/scripts/script.php?script_id=3412

hi Normal         ctermfg=16       ctermbg=NONE    cterm=NONE  guifg=Black guibg=White   gui=NONE
hi LineNr         ctermfg=16       ctermbg=255     cterm=NONE  guifg=Black guibg=#F5F5F5 gui=NONE

hi Comment        ctermfg=62       ctermbg=NONE    cterm=NONE  guifg=#559  guibg=NONE    gui=NONE
hi SpecialComment ctermfg=33       ctermbg=NONE    cterm=NONE  guifg=#37D  guibg=NONE    gui=NONE
hi Todo           ctermfg=16       ctermbg=Yellow  cterm=bold  guifg=Black guibg=Yellow  gui=bold

hi Constant       ctermfg=DarkRed  ctermbg=NONE    cterm=NONE  guifg=#A00  guibg=NONE    gui=NONE
hi Character      ctermfg=172      ctermbg=NONE    cterm=NONE  guifg=#B83  guibg=NONE    gui=NONE
hi String         ctermfg=172      ctermbg=NONE    cterm=NONE  guifg=#B83  guibg=NONE    gui=NONE
hi Number         ctermfg=DarkRed  ctermbg=NONE    cterm=NONE  guifg=#A00  guibg=NONE    gui=NONE
hi Float          ctermfg=DarkRed  ctermbg=NONE    cterm=NONE  guifg=#A00  guibg=NONE    gui=NONE
hi Boolean        ctermfg=DarkRed  ctermbg=NONE    cterm=NONE  guifg=#A00  guibg=NONE    gui=NONE

hi Identifier     ctermfg=28       ctermbg=NONE    cterm=bold  guifg=#1A0  guibg=NONE    gui=bold
hi Function       ctermfg=88       ctermbg=NONE    cterm=bold  guifg=#800  guibg=NONE    gui=bold

hi Statement      ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Conditional    ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Repeat         ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Label          ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Operator       ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Keyword        ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Exception      ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold

hi PreProc        ctermfg=196      ctermbg=NONE    cterm=bold  guifg=#E12  guibg=NONE    gui=bold
hi Include        ctermfg=196      ctermbg=NONE    cterm=bold  guifg=#E12  guibg=NONE    gui=bold
hi Define         ctermfg=196      ctermbg=NONE    cterm=bold  guifg=#E12  guibg=NONE    gui=bold
hi Macro          ctermfg=196      ctermbg=NONE    cterm=bold  guifg=#E12  guibg=NONE    gui=bold
hi PreCondit      ctermfg=196      ctermbg=NONE    cterm=bold  guifg=#E12  guibg=NONE    gui=bold

hi Type           ctermfg=16       ctermbg=NONE    cterm=NONE  guifg=Black guibg=NONE    gui=NONE
hi StorageClass   ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Structure      ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
hi Typedef        ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold

hi Special        ctermfg=93       ctermbg=NONE    cterm=NONE  guifg=#707  guibg=NONE    gui=NONE
hi SpecialChar    ctermfg=93       ctermbg=NONE    cterm=NONE  guifg=#707  guibg=NONE    gui=NONE
hi Tag            ctermfg=93       ctermbg=NONE    cterm=NONE  guifg=#707  guibg=NONE    gui=NONE
hi Delimiter      ctermfg=93       ctermbg=NONE    cterm=NONE  guifg=#707  guibg=NONE    gui=NONE
hi Debug          ctermfg=16       ctermbg=NONE    cterm=bold  guifg=Black guibg=NONE    gui=bold
