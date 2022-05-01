" vim syntax for empath
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>

if exists("b:current_syntax")
  finish
endif

syn cluster empathAtomCluster contains=empathAtomList,empathComment,empathTodo,empathFunc
syn cluster empathBaseListCluster contains=empathAtom,empathAtomMark,empathComment,empathTodo,empathFunc,empathList,empathNumber,empathSymbol

syn match empathSymbol contained ![^()'`,"; \t]\+!

syn match empathAtomMark "'"
syn match empathAtom "'[^ \t()]\+" contains=empathAtomMark
syn region empathAtom start=+'"+ skip=+\\"+ end=+"+
syn region empathAtomList contained matchgroup=Special start="(" matchgroup=Special end=")" contains=@empathAtomCluster,empathString

syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword empathFunc < <= = > >= + - ~ * / % ? ` : \\ , . A B X L Q && \|\| \| & ^ << >>
syn keyword empathFunc vec vecp ::: >>= case
syn keyword empathFunc twop nump symp tblp strp nilp homp ev ap
syn keyword empathFunc str slen sget scat ssub ystr sym fail
syn keyword empathFunc tbl tget tset tlen thas tkeys tdel

syn region empathString start=+"+ skip=+\\\\\|\\"+ end=+"+

syn cluster empathListCluster contains=@empathBaseListCluster,empathString

syn match empathNumber "\(+\|-\)*\(0d\|0D\)\?\(\.\d\+\|\d\+\(\.\d*\)\=\)"
syn match empathNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
syn match empathNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
syn match empathNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
syn match empathNumber "\(+\|-\)*\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"

syn match empathParenError ")"

syn match empathComment ";.*$" contains=empathTodo
syn match empathTodo "\(#.*$\|XXX\)"

syn sync lines=100

hi def link empathCommentRegion  empathComment
hi def link empathAtomMark       empathMark
hi def link empathInStringString empathString
hi def link empathAtom           Identifier
hi def link empathComment        Comment
hi def link empathFunc           Statement
hi def link empathMark           Delimiter
hi def link empathNumber         Number
hi def link empathParenError     Error
hi def link empathString         String
hi def link empathTodo           Todo

syn region empathList matchgroup=empathParen start="(" matchgroup=empathParen end=")" contains=@empathListCluster
hi def link empathParen Delimiter

let b:current_syntax = "empath"
