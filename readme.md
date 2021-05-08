# lips
lips is a simple lisp variant with a runtime written in C and
a self-hosting threaded code compiler that makes it faster than
most interpreted languages.

## build / install
are you on linux? `make` should work. otherwise consult the
makefile for the C compiler invocation. `make install` puts
files under `~/.local/{bin,lib}/`.

## special forms
nullary/unary cases are usually nil or identity, but `\`is an
exception. equivalents to examples are in scheme.

### `,` begin
- `(, a b c) = (begin a b c)`

### <code>\`</code> quote
- <code>(\` x) = (quote x)</code>

`'x` works too.

### `?` cond
- `(? a b c) = (cond (a b) (#t c))` if then else
- `(? a b) = (cond (a b) (#t #f))` if then

takes any number of branches. with no fallthrough branch the
implicit value is nil (`()`). nil evals to itself and is the
only false value.

### `:` define / letrec
- `(: a0 b0 ... an bn) = (begin (define a b) ... (define an bn) an)` even arguments : define variables in the current scope
- `(: a0 b0 ... an bn c) = (letrec ((a0 b0) ... (an bn)) c)` odd arguments : define variables and evaluate an expression in an inner scope
- `(: ((f g) x y) (g x y)) = (begin (define (f g) (lambda (x y) (g x y))) f)` nestable sugar for function defs

### `\` lambda
- `(\) = (lambda () #f)` nullary -> empty function
- `(\ (f x)) = (lambda () (f x))` unary -> a thunk
- `(\ a0 ... an x) = (lambda (a0 ... an) x)` however many arguments and one expression
- `(\ a b . (a b)) = (lambda (a . b) (a b))`  vararg syntax

`.` in the vararg syntax is a normal symbol with no special
meaning elsewhere (there are no pair literals). calling a
function with extra arguments is ok (not enough is an error).

## some predefined functions / macros
some of these are primitives in lips.c and some are defined in
prelude.lips.

- `L = list` `X = cons` `A = car` `B = cdr`.  `AA`-`BB` are macros.
- `+` `-` `*` `/` `%` what you think
- `<` `<=` `>=` `>` variadic, test each successive pair of arguments, works on numbers.
- `=` variadic, works on anything, recursive on pairs so `(= (L 1 2 3) (L 1 2 3))`.
- `ev = eval`, `ap = apply`, `ccc = call/cc`
- `cu` partial apply : `((cu f a b) c d) = (f a b c d)` `co` compose : `((co f g h) x) = (h (g (f x)))`
- `.` print arguments separated by spaces, print newline, return last argument
- `iota` and `rho` are kind of like in APL but not as good
- `homp` `nump` `twop` `symp` `nilp` `tblp` `strp` `vecp` type predicates
- hash functions: `tbl tset tget thas tkeys tlen tdel` ; tbl / tset take any number of key/value pairs
- string functions: n-ary constructor `(str 97 97 97) = "aaa"` ; `slen sget ssub scat`
- symbol functions: `gensym`
- `(::: nom0 def0 nom1 def1 ...)` define macro
- `(>>= x y z f) = (f x y z)` for readability

## code examples

### a quine
```lisp
((\ i (L i (L '` i))) '(\ i (L i (L '` i))))
```

### hyperoperations
```lisp
; send n to the nth hyperoperation where 0 is +
(: (hy n) (? (= n 0) + (\ x y (foldr1 (rho y x) (hy (- n 1))))))
```

### church numerals
```lisp
(: K const I id ; kind of like SK combinators
   (((P f) g) x) (f (g x)) ; normal composition
   (((Q f) g) x) ((P (f x)) (g x)) ; with an argument

   zero (K I) one I exp I mul P add Q succ (add one)

   (C n) (? (= n 0) zero (succ (C (- n 1)))) ; send it to its church numeral
   (N c) ((c (\ x (+ x 1))) 0))              ; send it back

(: (/p m n) (= 0 (% m n))
   (fizzbuzz m)
    ((K (+ m 1)) (. (?
     (/p m 15) 'fizzbuzz
     (/p m 5)  'buzz
     (/p m 3)  'fizz
     m)))
 (((C 100) fizzbuzz) 1))
```

## missing features
### type inference
under a weak type system suitable for dynamic languages.
this will catch many errors and reduce runtime checks.

### polymorphism / overloading
of functions like `+`, etc. this will need to fit in with the
type system.

### general purpose programming
wide characters, floats, arrays, files, networking, ...

### namespace / module system
important for ease of use and has practical benefits for
compiling.
