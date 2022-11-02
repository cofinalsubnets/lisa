" vim syntax for lisa
" based on lisp.vim by Charles E Campbell <http://www.drchip.org/astronaut/vim/index.html#SYNTAX_LISP>

if exists("b:current_syntax")
  finish
endif

syn cluster LisaAtomCluster contains=LisaAtomList,LisaComment,LisaTodo,LisaFunc
syn cluster LisaBaseListCluster contains=LisaAtom,LisaAtomMark,LisaComment,LisaTodo,LisaFunc,LisaList,LisaNumber,LisaSymbol

syn match LisaSymbol contained ![^()'`,"; \t]\+!

syn match LisaAtomMark "'"
syn match LisaAtom "'[^ \t()]\+" contains=LisaAtomMark
syn region LisaAtom start=+'"+ skip=+\\"+ end=+"+
syn region LisaAtomList contained matchgroup=Special start="(" matchgroup=Special end=")" contains=@LisaAtomCluster,LisaString

syn iskeyword @,!,37-38,42-47,:,60-63,\,`,|,~,^
syn keyword LisaFunc < <= = > >= + - ~ * / % ? ` : \\ , . A B X L Q && \|\| \| & ^ << >>
syn keyword LisaFunc ::: >>= case
syn keyword LisaFunc twop nump symp tblp strp nilp homp ev ap
syn keyword LisaFunc str slen schr scat ssub ystr sym fail
syn keyword LisaFunc tbl tget tset tlen thas tkeys tdel

syn region LisaString start=+"+ skip=+\\\\\|\\"+ end=+"+

syn cluster LisaListCluster contains=@LisaBaseListCluster,LisaString

" FIXME highlight signs right
syn match LisaNumber "\(+\|-\)*\(0d\|0D\)\?\(\.\d\+\|\d\+\(\.\d*\)\=\)"
syn match LisaNumber "\(+\|-\)*\(0b\|0B\)\(\.[01]\+\|[01]\+\(\.[01]*\)\=\)"
syn match LisaNumber "\(+\|-\)*\(0o\|0O\)\(\.\o\+\|\o\+\(\.\o*\)\=\)"
syn match LisaNumber "\(+\|-\)*\(0z\|0Z\)\(\.[0-9abAB]\+\|[0-9abAB]\+\(\.[0-9abAB]*\)\=\)"
syn match LisaNumber "\(+\|-\)*\(0x\|0X\)\(\.\x\+\|\x\+\(\.\x*\)\=\)"

syn match LisaParenError ")"

syn match LisaComment ";.*$" contains=LisaTodo
syn match LisaTodo "\(#.*$\|XXX\)"

syn sync lines=100

hi def link LisaCommentRegion  LisaComment
hi def link LisaAtomMark       LisaMark
hi def link LisaInStringString LisaString
hi def link LisaAtom           Identifier
hi def link LisaComment        Comment
hi def link LisaFunc           Statement
hi def link LisaMark           Delimiter
hi def link LisaNumber         Number
hi def link LisaParenError     Error
hi def link LisaString         String
hi def link LisaTodo           Todo

syn region LisaList matchgroup=LisaParen start="(" matchgroup=LisaParen end=")" contains=@LisaListCluster
hi def link LisaParen Delimiter

let b:current_syntax = "ll"
