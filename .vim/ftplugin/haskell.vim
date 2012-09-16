" Haskell Cuteness for Vim.
" Inspired by emacs-haskell-cuteness.
" Based on unilatex.vim by Jos van den Oever <oever@fenk.wau.nl>
"
" Changelog
"   0.1.3 - added more mappings and fixed bug in HaskellSrcToUTF8, thanks
"           to edwardkmett at Reddit
"   0.1.2 - added syntax highlighting as suggested by sfvisser at Reddit
"   0.1.1 - fixed stupid bug with haskell lambda expression
"   0.1 - initial release
"
" Version: 0.1.2
" Last Changed: 7 April 2009
" Maintainer: Andrey Popp <andrey.popp@braintrace.ru>

" Map to unicode symbols
imap <buffer> \ λ
imap <buffer> <- ←
imap <buffer> -> →
imap <buffer> <= ≲
imap <buffer> >= ≳
imap <buffer> == ≡
imap <buffer> /= ≠
imap <buffer> => ⇒
imap <buffer> >> »
imap <buffer> .<space> ∙<space>
imap <buffer> forall<space> ∀


" Turn syntax highlight on for new symbols
syn match hsVarSym "(\|λ\|←\|→\|≲\|≳\|≡\|≠\| )"

if exists("s:loaded_unihaskell")
	finish
endif
let s:loaded_unihaskell = 1

augroup HaskellC
	autocmd BufReadPost *.hs cal s:HaskellSrcToUTF8()
	autocmd BufWritePre *.hs cal s:UTF8ToHaskellSrc()
	autocmd BufWritePost *.hs cal s:HaskellSrcToUTF8()
augroup END

" function to convert ''fancy haskell source'' to haskell source
function s:UTF8ToHaskellSrc()
	let s:line = line(".")
	let s:column = col(".")

	silent %s/λ/\\/eg
	silent %s/←/<-/eg
	silent %s/→/->/eg
	silent %s/≲/<=/eg
	silent %s/≳/>=/eg
    silent %s/≡/==/eg
    silent %s/≠/\/=/eg
    silent %s/⇒/=>/eg
    silent %s/»/>>/eg
    silent %s/∙ /. /eg
    silent %s/∀/forall /eg


	let &l:fileencoding = s:oldencoding
	call cursor(s:line,s:column)
endfunction

" function to convert haskell source to ''fancy haskell source''
function s:HaskellSrcToUTF8()
	let s:line = line(".")
	let s:column = col(".")

	let s:oldencoding = &l:fileencoding
	set fileencoding=utf-8

    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=\\\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/λ\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=->\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/→\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=<-\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/←\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=<=\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≲\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=>=\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≳\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<===\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≡\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=\/=\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/≠\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<==>\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/⇒\1/eg
    silent %s/[^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>?@\^|~.]\@<=>>\([^λ←→≲≳≡≠⇒»∙∀\\\-!#$%&*+/<=>\?@\^|~.]\)/»\1/eg
    silent %s/forall /∀/eg
    silent %s/ \@<=\. /∙ /eg
    
	let &l:fileencoding = s:oldencoding
	call cursor(s:line,s:column)
endfunction

do HaskellC BufRead
