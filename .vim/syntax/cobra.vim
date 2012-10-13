" Vim syntax file
" Language:     Cobra
" Maintainer:   Ramon Rocha <ramon.rocha@live.com>
" Last Change:  02 Jul 2012
" Remark:       Based on version created by Todd Alexander (see
" http://bitbucket.org/webnov8/cobra-on-vim)

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif


syn keyword cobraModule       use import namespace nextgroup=cobraNamespace skipwhite
syn match   cobraNamespace    "[A-Z][a-zA-Z0-9_]*\(\(\.\)[A-Z][a-zA-Z0-9_]*\)*" contained
hi def link cobraModule       Statement
hi def link cobraNamespace    Identifier

syn match   cobraPreProc      "@args\|@number\|@ref"
syn match   cobraPreProc      "@help\|@throw\|@error\|@warning"
hi def link cobraPreProc      PreProc

syn keyword cobraSection      require ensure invariant test body
syn keyword cobraException    try catch finally throw success
hi def link cobraSection      Debug
hi def link cobraException    Exception

syn keyword cobraBoxDec       class interface struct extend enum mixin nextgroup=cobraCamelCase skipwhite
syn match   cobraCamelCase    "[A-Z][a-zA-Z0-9_]*" contained
hi def link cobraBoxDec       Structure
hi def link cobraCamelCase    Identifier

syn keyword cobraKeyword      result old this base
syn keyword cobraKeyword      shared private protected virtual abstract
syn keyword cobraKeyword      vari shared event
hi def link cobraKeyword      Keyword

syn keyword cobraStatement    post break continue off
syn keyword cobraStatement    pass print stop trace yield listen ignore end do ref
syn keyword cobraStatement    assert expect return raise nextgroup=cobraCamelCase2 skipwhite
hi def link cobraStatement    Statement

syn keyword cobraDec          def cue get set pro sig nextgroup=cobraFunction skipwhite
syn match   cobraFunction     "[a-zA-Z_][a-zA-Z0-9_]*" contained
syn keyword cobraDec          var from const using nextgroup=cobraIdentifier skipwhite
syn match   cobraIdentifier   "[a-zA-Z_][a-zA-Z0-9_]*" contained
hi def link cobraDec          Statement
hi def link cobraFunction     Function
hi def link cobraIdentifier   Identifier

" This next match ensures identifies highlight correctly in comma-delimited
" lists.
syn match   cobraNormal       ",\|(" nextgroup=cobraCamelCase2 skipwhite
hi def link cobraNormal       Normal

" Not sure what else to categorize this as...
syn keyword cobraTypedef      as to nextgroup=cobraType,cobraTypeName,cobraCamelCase2 skipwhite
hi def link cobraTypedef      Typedef

syn keyword cobraOperator     is in not of where must be and or all out implies inout callable nextgroup=cobraCamelCase2 skipwhite
syn match   cobraOperator     "<>\|==\|=\|<\|>" nextgroup=cobraCamelCase2 skipwhite
syn match   cobraCamelCase2   "[A-Z][a-zA-Z0-9_]*" contained
hi def link cobraOperator     Operator
hi def link cobraCamelCase2   Type

syn keyword cobraDeclaration  implements inherits nextgroup=cobraTypeName skipwhite
syn match   cobraTypeName     "[a-zA-Z_][a-zA-Z0-9_.]*" contained
syn keyword cobraModifier     public private protected abstract virtual extern
syn keyword cobraModifier     partial override internal new
hi def link cobraDeclaration  Keyword
hi def link cobraTypeName     Type
hi def link cobraModifier     Keyword

syn keyword cobraType         bool char decimal float float32 int8 int16 int int64 uint8 uint16 uint uint64
syn keyword cobraType         number dynamic same
hi def link cobraType         Type

syn keyword cobraConditional  if else branch nextgroup=cobraCamelCase2 skipwhite
syn keyword cobraLabel        on nextgroup=cobraCamelCase2 skipwhite
syn keyword cobraRepeat       while for nextgroup=cobraCamelCase2 skipwhite
hi def link cobraConditional  Conditional
hi def link cobraLabel        Label
hi def link cobraRepeat       Repeat

syn region  cobraGeneric      start=+<of+ end=+>+ contains=cobraGeneric,cobraType,cobraCamelCase2
hi def link cobraGeneric      Special

" Strings must come before DocStrings or things don't work quite right
syn region  cobraString       matchgroup=String start=+r\='+ end=+'+ skip=+\\\\\|\\'+ contains=cobraEscape,cobraNoSub,cobraSub
syn region  cobraString       matchgroup=String start=+r\="+ end=+"+ skip=+\\\\\|\\"+ contains=cobraEscape,cobraNoSub,cobraSub
hi def link cobraString       String

syn keyword cobraTodo         TODO FIXME HACK hack contained
syn match   cobraTodo         "to-do" contained
hi def link cobraTodo         Todo

syn match   cobraComment      "#.*$" contains=cobraTodo
syn region  cobraComment      matchgroup=Comment start=+\/\#+ end=+\#\/+ contains=cobraTodo
hi def link cobraComment      Comment

syn region  cobraDocString    matchgroup=SpecialComment start=+"""+ end=+"""+ contains=cobraTodo
syn region  cobraDocString    matchgroup=SpecialComment start=+'''+ end=+'''+ contains=cobraTodo
hi def link cobraDocString    SpecialComment

syn keyword cobraBoolean      true false
syn keyword cobraConstant     nil
hi def link cobraBoolean      Boolean
hi def link cobraConstant     Constant

syn match   cobraNumber       "\<0x\x\+[Ll]\=\>"
syn match   cobraNumber       "\<\d\+[Ll]\=\>"
syn match   cobraNumber       "\.\d\+\([eE][+-]\=\d\+\)\=\>"
syn match   cobraNumber       "\<\d\+\.\([eE][+-]\=\d\+\)\=\>"
syn match   cobraNumber       "\<\d\+\.\d\+\([eE][+-]\=\d\+\)\=\>"
hi def link cobraNumber       Number

syn match   cobraEscape       +\\[abfnrtv'"\\]+ contained
syn match   cobraEscape       "\\\o\{1,3}" contained
syn match   cobraEscape       "\\x\x\{2}" contained
syn match   cobraEscape       "\(\\u\x\{4}\|\\U\x\{8}\)" contained
syn match   cobraEscape       "\\$"
syn match   cobraSubText      "\(_.\)\=[a-zA-Z][a-zA-Z0-9_]" contained
syn match   cobraSub          "\[[.a-zA-Z0-9_]\+\]" contained
syn match   cobraNoSub        "\\\[[.a-zA-Z0-9_]\+\(\[[a-zA-Z0-9_]\+\]\)*\]" contained
hi def link cobraEscape       Special
hi def link cobraSubText      String
hi def link cobraSub          Delimiter
hi def link cobraNoSub        String

" Adding the sync limits back in otherwise doc-strings are not
" highlighted correctly when performing searches in vim.
syn sync maxlines=200
syn sync minlines=500

let b:current_syntax = "cobra"
