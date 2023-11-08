# gwen
another list processing style programming language

## how to build
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## how to use
- false = nil = 0
- evaluation is applicative order
- functions are curried
- data are own constant functions
- singleton list is `quote`
- `(\ arg1 arg2 arg3 exp)` is like `lambda`
- `(? test1 exp1 test2 exp2 test3 exp3)` is like `cond`
- `(: a x b y c z (a b c))` is like `letrec`
- `(, a b c d)` is like `begin`
