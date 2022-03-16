" vim syntax for lips
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>

if exists("b:current_syntax")
  finish
endif

syn cluster lipsAtomCluster contains=lipsAtomList,lipsComment,lipsTodo,lipsFunc
syn cluster lipsBaseListCluster contains=lipsAtom,lipsAtomMark,lipsComment,lipsTodo,lipsFunc,lipsList,lipsNumber,lipsSymbol

syn match lipsSymbol contained ![^()'`,"; \t]\+!

syn match lipsAtomMark "'"
syn match lipsAtom "'[^ \t()]\+" contains=lipsAtomMark
syn region lipsAtom start=+'"+ skip=+\\"+ end=+"+
syn region lipsAtomList contained matchgroup=Special start="(" matchgroup=Special end=")" contains=@lipsAtomCluster,lipsString

syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword lipsFunc < <= = > >= + - ~ * / % ? ` : \\ , . A B X L Q && \|\| \| & ^ << >>
syn keyword lipsFunc vec vecp ::: >>= case
syn keyword lipsFunc twop nump symp tblp strp nilp homp ev ap
syn keyword lipsFunc str slen sget scat ssub ystr sym fail
syn keyword lipsFunc tbl tget tset tlen thas tkeys tdel

syn region lipsString start=+"+ skip=+\\\\\|\\"+ end=+"+

syn cluster lipsListCluster contains=@lipsBaseListCluster,lipsString

syn match lipsNumber "\(+\|-\)*\(0d\|0D\)\?\(\.\d\+\|\d\+\(\.\d*\)\=\)"
syn match lipsNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
syn match lipsNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
syn match lipsNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
syn match lipsNumber "\(+\|-\)*\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"

syn match lipsParenError ")"

syn match lipsComment ";.*$" contains=lipsTodo
syn match lipsTodo "\(#.*$\|XXX\)"

syn sync lines=100

hi def link lipsCommentRegion  lipsComment
hi def link lipsAtomMark       lipsMark
hi def link lipsInStringString lipsString
hi def link lipsAtom           Identifier
hi def link lipsComment        Comment
hi def link lipsFunc           Statement
hi def link lipsMark           Delimiter
hi def link lipsNumber         Number
hi def link lipsParenError     Error
hi def link lipsString         String
hi def link lipsTodo           Todo

syn region lipsList matchgroup=lipsParen start="(" matchgroup=lipsParen end=")" contains=@lipsListCluster
hi def link lipsParen Delimiter

let b:current_syntax = "lips"
