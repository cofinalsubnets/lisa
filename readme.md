# lips
trivial lisp

## qualities
- strict lexically scoped lisp-1 with short function words
  and no superfluous parentheses
- self-hosting threaded code compiler targets a vm/runtime
  written in somewhat portable C (64-bit only, etc.)
- some kind of weak static type concept

## build / install
are you on linux? `make` should work. otherwise consult the
makefile for the C compiler invocation. `make install` puts
the binary and prelude under `~/.local` by default.

## special forms
with scheme equivalents. degenerate (nullary/unary) cases
are usually nil or identity, but `\`is an exception

### `,` begin
- `(, a b c) = (begin a b c)`

like scheme.

### <code>\`</code> quote
- <code>(\` x) = (quote x)</code>

like scheme. `'x` also works.

### `?` cond
- `(? a b) = (cond (a b) (#t '()))` even case : default value is nil
- `(? a b c) = (cond (a b) (#t c))` odd case : default branch is given

etc. `()` is the only false value & it's self-quoting.

### `:` define / let
- `(: a0 b0 ... an bn) = (begin (define a b) ... (define an bn) an)` even arguments : define variables in the current scope
- `(: a0 b0 ... an bn c) = (letrec ((a0 b0) ... (an bn)) c)` odd arguments : define variables and evaluate an expression in an inner scope
- `(: (a b c) (b c)) = (begin (define (a b c) (b c)) a)` function symbols : scheme-like syntactic sugar for defining functions

### `\` lambda
- `(\) = (lambda () '())` nullary case is an empty function
- `(\ a) = (lambda () a)` unary case is a constant function
- `(\ a0 ... an x) = (lambda (a0 ... an) x)` many arguments, one expression
- `(\ a b . (a b)) = (lambda (a . b) (a b))`  vararg syntax ; `.` is just a symbol

use `,` to sequence multiple expressions in one function body.

## predefined functions / macros
this whole section is unstable and  some of these names are
bad, sorry, you know what they say about naming things!
many of these, and others not listed here, are defined in
`prelude.lips`

- `+` `-` `*` `/` `%` what you probably think!
- `<` `<=` `>=` `>` variadic, test each successive pair of
  arguments, works on numbers.
- `=` variadic, works on anything, recursive on pairs so
  `(= (L 1 2 3) (L 1 2 3))`.
- `ev = eval`, `ap = apply`, `ccc = call/cc`
- `.` print arguments separated by spaces, print newline, return
  last argument; great for debugging.
- `A = car` `B = cdr` `X = cons` `L = list`. `AA`-`BB` are
  defined as macros.
- apl-lite data constructors: `iota` is monadic `ι`; `rho` is
  a weaker dyadic `ρ`: `(ap rho (X n l))` concatenates n copies
  of l.
- `homp` `nump` `twop` `symp` `nilp` `tblp` `strp` `vecp` type predicates
- hash functions: `tbl tset tget thas tkeys tlen tdel` ; see prelude.lips for usage
- string functions: n-ary constructor `(str 97 97 97) = "aaa"` ; `slen sget ssub scat`
- symbol functions: `gensym`
- `(::: nom0 def0 nom1 def1 ...)` define macro
- `(>>= x y z f) = (f x y z)`

## code examples

### hyperoperations
```lisp
; send n to the nth hyperoperation, 0 being addition.
(: (hy n) (? (= n 0) + (\ x y
 (foldr1 (rho y x) (hy (- n 1))))))
```

### church numerals
```lisp
(: K const ; these are just slightly modified SK combinators
   I id
   (P f) (\ g (\ x (f (g x))))         ; postcomposition
   (Q x) (\ y (\ z ((P (x z)) (y z)))) ; Q : S :: compose : apply

   zero (K I) one I exp I mul P add Q succ (add one) ; okie dokie

   (C n) (? (= n 0) zero (succ (C (- n 1)))) ; send it to its church numeral
   (N c) ((c (\ x (+ x 1))) 0))              ; send it back
```

### fizzbuzz
```lisp
; now that we have church numerals we can write fizzbuzz.
(: (/p m n) (= 0 (% m n))
   (fb m) (, (. (? (/p m 15) 'fizzbuzz
                   (/p m 5)  'buzz
                   (/p m 3)  'fizz
                   m))
             (+ m 1))
 (((C 100) fb) 1))
```

### a quine
```lisp
((\ i (L i (L '` i))) '(\ i (L i (L '` i))))
```

## missing features
- arrays, floats and many other types
- unicode
- useful i/o
- namespaces / module system
