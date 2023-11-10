# gwen
another list processing style programming language

- there is no false or nil, just 0
- there are no dotted lists, the last cdr just is not shown
- functions are curried, there are no variadic functions (but there are macros)
- application is left to right one argument at a time
- data are own constant functions
- singleton list is `quote`, there are no nullary functions (just ignore it :)
- `(, a b c d)` is like `begin`
- `(\ a b c x)` is `lambda` (`a b c` are arguments and `x` is body, use `,` for multiple expressions)
- `(? a b c d e f g)` is `cond` (g is default branch)
- `(: a x b y c z (a b c))` is like `letrec` (if no expression then return last definition)
