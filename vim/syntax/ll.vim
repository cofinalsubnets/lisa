" vim syntax for ll
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>

if exists("b:current_syntax")
  finish
endif

syn cluster LlAtomCluster contains=LlAtomList,LlComment,LlTodo,LlFunc
syn cluster LlBaseListCluster contains=LlAtom,LlAtomMark,LlComment,LlTodo,LlFunc,LlList,LlNumber,LlSymbol

syn match LlSymbol contained ![^()'`,"; \t]\+!

syn match LlAtomMark "'"
syn match LlAtom "'[^ \t()]\+" contains=LlAtomMark
syn region LlAtom start=+'"+ skip=+\\"+ end=+"+
syn region LlAtomList contained matchgroup=Special start="(" matchgroup=Special end=")" contains=@LlAtomCluster,LlString

syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword LlFunc < <= = > >= + - ~ * / % ? ` : \\ , . A B X L Q && \|\| \| & ^ << >>
syn keyword LlFunc ::: >>= case
syn keyword LlFunc twop nump symp tblp strp nilp homp ev ap
syn keyword LlFunc str slen sget scat ssub ystr sym fail
syn keyword LlFunc tbl tget tset tlen thas tkeys tdel

syn region LlString start=+"+ skip=+\\\\\|\\"+ end=+"+

syn cluster LlListCluster contains=@LlBaseListCluster,LlString

syn match LlNumber "\(+\|-\)*\(0d\|0D\)\?\(\.\d\+\|\d\+\(\.\d*\)\=\)"
syn match LlNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
syn match LlNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
syn match LlNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
syn match LlNumber "\(+\|-\)*\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"

syn match LlParenError ")"

syn match LlComment ";.*$" contains=LlTodo
syn match LlTodo "\(#.*$\|XXX\)"

syn sync lines=100

hi def link LlCommentRegion  LlComment
hi def link LlAtomMark       LlMark
hi def link LlInStringString LlString
hi def link LlAtom           Identifier
hi def link LlComment        Comment
hi def link LlFunc           Statement
hi def link LlMark           Delimiter
hi def link LlNumber         Number
hi def link LlParenError     Error
hi def link LlString         String
hi def link LlTodo           Todo

syn region LlList matchgroup=LlParen start="(" matchgroup=LlParen end=")" contains=@LlListCluster
hi def link LlParen Delimiter

let b:current_syntax = "ll"
