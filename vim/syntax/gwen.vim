" vim syntax for gwen
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>

if exists("b:current_syntax")
  finish
endif

syn cluster GwenAtomCluster contains=GwenAtomList,GwenComment,GwenTodo,GwenFunc
syn cluster GwenBaseListCluster contains=GwenAtom,GwenAtomMark,GwenComment,GwenTodo,GwenFunc,GwenList,GwenNumber,GwenSymbol

syn match GwenSymbol contained ![^()'`,"; \t]\+!

syn match GwenAtomMark "'"
syn match GwenAtom "'[^ \t()]\+" contains=GwenAtomMark
syn region GwenAtom start=+'"+ skip=+\\"+ end=+"+
syn region GwenAtomList contained matchgroup=Special start="(" matchgroup=Special end=")" contains=@GwenAtomCluster,GwenString

syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword GwenFunc < <= = > >= + - ~ ! * / % ? ` : \\ , . A B AA AB BA BB X L Q && \|\| \| & ^ << >>
"syn keyword GwenFunc ::: >>= case
"syn keyword GwenFunc twop nump symp tblp strp nilp homp ev ap
"syn keyword GwenFunc str slen schr scat ssub ystr sym nope
"syn keyword GwenFunc tbl tget tset tlen thas tkeys tdel

syn region GwenString start=+"+ skip=+\\\\\|\\"+ end=+"+

syn cluster GwenListCluster contains=@GwenBaseListCluster,GwenString

" FIXME highlight signs right
syn match GwenNumber "\(+\|-\)*\(0d\|0D\)\?\(\.\d\+\|\d\+\(\.\d*\)\=\)"
syn match GwenNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
syn match GwenNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
syn match GwenNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
syn match GwenNumber "\(+\|-\)*\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"

syn match GwenParenError ")"

syn match GwenComment ";.*$" contains=GwenTodo
syn match GwenTodo "\(#.*$\|XXX\|TODO\|FIXME\)"

syn sync lines=100

hi def link GwenCommentRegion  GwenComment
hi def link GwenAtomMark       GwenMark
hi def link GwenInStringString GwenString
hi def link GwenAtom           Identifier
hi def link GwenComment        Comment
hi def link GwenFunc           Statement
hi def link GwenMark           Delimiter
hi def link GwenNumber         Number
hi def link GwenParenError     Error
hi def link GwenString         String
hi def link GwenTodo           Todo

syn region GwenList matchgroup=GwenParen start="(" matchgroup=GwenParen end=")" contains=@GwenListCluster
hi def link GwenParen Delimiter

let b:current_syntax = "gwen"
