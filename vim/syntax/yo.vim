" vim syntax for yo
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>

if exists("b:current_syntax")
  finish
endif

syn cluster yoAtomCluster contains=yoAtomList,yoComment,yoTodo,yoFunc
syn cluster yoBaseListCluster contains=yoAtom,yoAtomMark,yoComment,yoTodo,yoFunc,yoList,yoNumber,yoSymbol

syn match yoSymbol contained ![^()'`,"; \t]\+!

syn match yoAtomMark "'"
syn match yoAtom "'[^ \t()]\+" contains=yoAtomMark
syn region yoAtom start=+'"+ skip=+\\"+ end=+"+
syn region yoAtomList contained matchgroup=Special start="(" matchgroup=Special end=")" contains=@yoAtomCluster,yoString

syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword yoFunc < <= = > >= + - ~ * / % ? ` : \\ , . A B X L Q && \|\| \| & ^ << >>
syn keyword yoFunc ::: >>= case
syn keyword yoFunc twop nump symp tblp strp nilp homp ev ap
syn keyword yoFunc str slen sget scat ssub ystr sym fail
syn keyword yoFunc tbl tget tset tlen thas tkeys tdel

syn region yoString start=+"+ skip=+\\\\\|\\"+ end=+"+

syn cluster yoListCluster contains=@yoBaseListCluster,yoString

syn match yoNumber "\(+\|-\)*\(0d\|0D\)\?\(\.\d\+\|\d\+\(\.\d*\)\=\)"
syn match yoNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
syn match yoNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
syn match yoNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
syn match yoNumber "\(+\|-\)*\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"

syn match yoParenError ")"

syn match yoComment ";.*$" contains=yoTodo
syn match yoTodo "\(#.*$\|XXX\)"

syn sync lines=100

hi def link yoCommentRegion  yoComment
hi def link yoAtomMark       yoMark
hi def link yoInStringString yoString
hi def link yoAtom           Identifier
hi def link yoComment        Comment
hi def link yoFunc           Statement
hi def link yoMark           Delimiter
hi def link yoNumber         Number
hi def link yoParenError     Error
hi def link yoString         String
hi def link yoTodo           Todo

syn region yoList matchgroup=yoParen start="(" matchgroup=yoParen end=")" contains=@yoListCluster
hi def link yoParen Delimiter

let b:current_syntax = "yo"
