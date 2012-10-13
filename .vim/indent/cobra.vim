" Vim indent file
" Language:   Cobra
" Maintainer: Todd Alexander
" Last Change: 2009-07-02

" Only load this indent file when no other was loaded.
if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

" Some preliminary settings
setlocal indentexpr=GetCobraIndent()
setlocal indentkeys=!^F,o,O,e

" Best practices for the language is to use hard tabs
" then let your editor control the display size of those tabs.
set sw=2 ts=2 noexpandtab

if exists("*GetCobraIndent")
  finish
endif

function! GetCobraIndent()
	let line = getline(v:lnum) " current line
  let lnum = prevnonblank(v:lnum - 1) " previous non-blank line number

  if lnum == 0
    return 0
  endif
  
  let ind = indent(lnum) " previous line's indent
  let prevline = getline(lnum)

  if prevline =~ '^\s*\<\(namespace\|shared\|class\|cue\|def\|struct\|enum' .
    \ '\|for\|while\|if\|else\|try\|catch\|finally\|break\|continue\|private\|protected\|branch\|on\)\>'
    let ind += &sw
  endif

  if prevline =~ '^\s*\<\(break\|continue\|pass\|throw\|return\)\>'
    let ind -= &sw
  endif

  return ind
endfunction

" vim: sw=2 ts=2
