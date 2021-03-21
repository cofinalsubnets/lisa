# lips
low-level virtual lisp machine and self-hosting compiler.
tested on x86 and ARM.

## build / install
are you on linux? `make` should work. otherwise consult the
makefile for the C compiler invocation. `make install`
puts the binary and prelude under `~/.local` by default.

## special forms
with scheme equivalents
### `,` begin
- `(,) = '()`
- `(, a) = a`
- `(, a b) = (begin a b)`

### `:` define / let
- `(:) = '()`
- `(: a) = a`
- `(: a b) = (begin (define a b) a)`
- `(: a b c d) = (begin (define a b) (define c d) c)`
- `(: a b c d e) = (letrec ((a b) (c d)) e)`

### `\` lambda
- `(\) = (lambda () ())`
- `(\ a) = (lambda () a)`
- `(\ a a) = (lambda (a) a)`
- `(\ a b (a b)) = (lambda (a b) (a b))`
- `(\ a b . (a b)) = (lambda (a . b) (a b))`
- `(\ a b c (, x y z)) = (lambda (a b c) x y z)`

### `?` cond
- `(?) = '()`
- `(? a) = a`
- `(? a b) = (cond (a b) (#t '()))`
- `(? a b c) = (cond (a b) (#t c))`
- `(? a b c d) = (cond (a b) (c d) (#t '()))`

nil is the only false value

### <code>\`</code> quote
- <code>(\`) = '()</code>
- <code>(\` x) = (quote x)</code>

`'x` also works.

## functions
this whole section is unstable and  some of these names are
bad, sorry, you know what they say about naming things!
- `+` `-` `*` `/` `%` what you probably think!
- `<` `<=` `>=` `>` variadic, test each successive pair of
  arguments, works on numbers.
- `=` variadic, works on anything, recursive on pairs so
  `(= (L 1 2 3) (L 1 2 3))`.
- `ev = eval`, `ap = apply`, `ccc = call/cc`
- `.` print arguments separated by spaces, print newline, return
  last argument; useful for debugging.
- `A = car` `B = cdr` `X = cons` `L = list`. `AA`-`BB` are
  defined as macros.
- `homp` `nump` `twop` `symp` `nilp` `tblp` `strp` `vecp` type predicates
- hash functions: `tbl tset tget thas tkeys tlen tdel` ; see prelude.lips for usage
- string functions: n-ary constructor `(str 97 97 97) = "aaa"` ; `slen sget ssub scat`
- symbol functions: `gensym`

## macros
these are defined in `prelude.lips`, `make repl` imports them automatically
- `(::: nom0 def0 nom1 def1 ...)` define macro
- `(>>= x y z f) = (f x y z)` bind ; `(let ((a x) (b y) (c z)) q) = (>>= x y z (\ a b c q))`

## code examples
the thread compiler and built-in functions are in `prelude.lisp`.

### fizzbuzz
```lisp
(: /p (\ m n (= 0 (% m n)))
   fb (\ m n (? (<= m n) (,
     (. (? (/p m 15) 'fb
           (/p m 5)  'b
           (/p m 3)  'f
           m))
     (fb (+ m 1) n))))
   (fb 1 100))
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
