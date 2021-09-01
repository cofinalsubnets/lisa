" vim syntax file for lips
" shamelessly cribbed from lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>
" the rainbow thing is genius but i don't even know vimscript

if exists("b:current_syntax")
 finish
endif

syn cluster   lipsAtomCluster  contains=lipsAtomBarSymbol,lipsAtomList,lipsAtomNmbr0,lipsComment,lipsTodo,lipsDecl,lipsFunc,lipsLeadWhite
syn cluster   lipsBaseListCluster contains=lipsAtom,lipsAtomBarSymbol,lipsAtomMark,lipsBQList,lipsBarSymbol,lipsComment,lipsTodo,lipsConcat,lipsDecl,lipsFunc,lipsKey,lipsList,lipsNumber,lipsEscapeSpecial,lipsSymbol,lipsVar,lipsLeadWhite
if exists("g:lips_instring")
 syn cluster   lipsListCluster  contains=@lipsBaseListCluster,lipsString,lipsInString,lipsInStringString
else
 syn cluster   lipsListCluster  contains=@lipsBaseListCluster,lipsString
endif

syn match lipsSymbol contained ![^()'`,"; \t]\+!
syn match lipsBarSymbol contained !|..\{-}|!
if exists("g:lips_rainbow") && g:lips_rainbow != 0
  syn region lipsParen0           matchgroup=hlLevel0 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen1
  syn region lipsParen1 contained matchgroup=hlLevel1 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen2
  syn region lipsParen2 contained matchgroup=hlLevel2 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen3
  syn region lipsParen3 contained matchgroup=hlLevel3 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen4
  syn region lipsParen4 contained matchgroup=hlLevel4 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen5
  syn region lipsParen5 contained matchgroup=hlLevel5 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen6
  syn region lipsParen6 contained matchgroup=hlLevel6 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen7
  syn region lipsParen7 contained matchgroup=hlLevel7 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen8
  syn region lipsParen8 contained matchgroup=hlLevel8 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen9
  syn region lipsParen9 contained matchgroup=hlLevel9 start="`\=(" end=")" skip="|.\{-}|" contains=@lipsListCluster,lipsParen0
else
  syn region lipsList   matchgroup=lipsParen start="("   skip="|.\{-}|"   matchgroup=lipsParen end=")" contains=@lipsListCluster
  syn region lipsBQList   matchgroup=PreProc   start="`("  skip="|.\{-}|"   matchgroup=PreProc   end=")"  contains=@lipsListCluster
endif

" ---------------------------------------------------------------------
syn match lipsAtomMark   "'"
syn match lipsAtom   "'("me=e-1   contains=lipsAtomMark nextgroup=lipsAtomList
syn match lipsAtom   "'[^ \t()]\+"   contains=lipsAtomMark
syn match lipsAtomBarSymbol  !'|..\{-}|!   contains=lipsAtomMark
syn region lipsAtom   start=+'"+   skip=+\\"+ end=+"+
syn region lipsAtomList   contained   matchgroup=Special start="(" skip="|.\{-}|" matchgroup=Special end=")" contains=@lipsAtomCluster,lipsString,lipsEscapeSpecial
syn match lipsAtomNmbr   contained   "\<\d\+"
syn match lipsLeadWhite   contained   "^\s\+"

" ---------------------------------------------------------------------
syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~
syn keyword lipsFunc < <= = > >= + - ~ * / % ? ` : \\ , . A B X L Q && \|\|
syn keyword lipsFunc vecp ::: >>= case
syn keyword lipsFunc twop nump symp tblp strp nilp homp ev ap
syn keyword lipsFunc str slen sget scat ssub ssym ystr ygen fail
syn keyword lipsFunc tbl tget tset tlen thas tkeys tdel


syn region lipsString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell
if exists("g:lips_instring")
  syn region lipsInString keepend matchgroup=Delimiter start=+"(+rs=s+1 skip=+|.\{-}|+ matchgroup=Delimiter end=+)"+ contains=@lipsBaseListCluster,lipsInStringString
  syn region lipsInStringString start=+\\"+ skip=+\\\\+ end=+\\"+ contained
endif

" ---------------------------------------------------------------------
" ---------------------------------------------------------------------
syn match lipsNumber  "-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
syn match lipsNumber  "-\=\(\d\+/\d\+\)"

syn match lipsEscapeSpecial  "\*\w[a-z_0-9-]*\*"
syn match lipsEscapeSpecial  !#|[^()'`,"; \t]\+|#!
syn match lipsEscapeSpecial  !#x\x\+!
syn match lipsEscapeSpecial  !#o\o\+!
syn match lipsEscapeSpecial  !#b[01]\+!
syn match lipsEscapeSpecial  !#\\[ -}\~]!
syn match lipsEscapeSpecial  !#[':][^()'`,"; \t]\+!
syn match lipsEscapeSpecial  !#([^()'`,"; \t]\+)!
syn match lipsEscapeSpecial  !#\\\%(Space\|Newline\|Tab\|Page\|Rubout\|Linefeed\|Return\|Backspace\)!
syn match lipsEscapeSpecial  "\<+[a-zA-Z_][a-zA-Z_0-9-]*+\>"

syn match lipsConcat "\s\.\s"
syn match lipsParenError ")"

syn cluster lipsCommentGroup contains=@Spell
syn match lipsComment ";.*$" contains=@lipsCommentGroup
syn match lipsTodo "#.*$" contains=@lipsCommentGroup

" ---------------------------------------------------------------------
syn sync lines=100

" ---------------------------------------------------------------------
if !exists("skip_lips_syntax_inits")

  hi def link lipsCommentRegion  lipsComment
  hi def link lipsAtomNmbr  lipsNumber
  hi def link lipsAtomMark  lipsMark
  hi def link lipsInStringString lipsString

  hi def link lipsAtom   Identifier
  hi def link lipsAtomBarSymbol  Special
  hi def link lipsBarSymbol  Special
  hi def link lipsComment  Comment
  hi def link lipsConcat  Statement
  hi def link lipsDecl   Statement
  hi def link lipsFunc   Statement
  hi def link lipsKey   Type
  hi def link lipsMark   Delimiter
  hi def link lipsNumber  Number
  hi def link lipsParenError  Error
  hi def link lipsEscapeSpecial  Type
  hi def link lipsString  String
  hi def link lipsTodo   Todo
  hi def link lipsVar   Statement

  if exists("g:lips_rainbow") && g:lips_rainbow != 0
   if &bg == "dark"
    hi def hlLevel0 ctermfg=red  guifg=red1
    hi def hlLevel1 ctermfg=yellow guifg=orange1
    hi def hlLevel2 ctermfg=green guifg=yellow1
    hi def hlLevel3 ctermfg=cyan guifg=greenyellow
    hi def hlLevel4 ctermfg=magenta guifg=green1
    hi def hlLevel5 ctermfg=red  guifg=springgreen1
    hi def hlLevel6 ctermfg=yellow guifg=cyan1
    hi def hlLevel7 ctermfg=green guifg=slateblue1
    hi def hlLevel8 ctermfg=cyan guifg=magenta1
    hi def hlLevel9 ctermfg=magenta guifg=purple1
   else
    hi def hlLevel0 ctermfg=red  guifg=red3
    hi def hlLevel1 ctermfg=darkyellow guifg=orangered3
    hi def hlLevel2 ctermfg=darkgreen guifg=orange2
    hi def hlLevel3 ctermfg=blue guifg=yellow3
    hi def hlLevel4 ctermfg=darkmagenta guifg=olivedrab4
    hi def hlLevel5 ctermfg=red  guifg=green4
    hi def hlLevel6 ctermfg=darkyellow guifg=paleturquoise3
    hi def hlLevel7 ctermfg=darkgreen guifg=deepskyblue4
    hi def hlLevel8 ctermfg=blue guifg=darkslateblue
    hi def hlLevel9 ctermfg=darkmagenta guifg=darkviolet
   endif
  else
    hi def link lipsParen Delimiter
  endif

endif

let b:current_syntax = "lips"

" ---------------------------------------------------------------------
" vim: ts=8 nowrap fdm=marker
