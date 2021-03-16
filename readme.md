# lips
## usage
`make install` installs files into the `~/.local` hierarchy.
assuming `~/.local/bin` is in your `$PATH` `lips` should start
a repl.

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
- `(: a b c d e) = (let* ((a b) (c d)) e)`

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
- `ev = eval` `ap = apply` `ccc = call/cc`
- `.` print arguments separated by spaces then newline
- `+` `-` `*` `/` `%` what you think
- `&&` `||` left-to-right, shortcut eval when bound early.
- `<` `<=` `=` `>=` `>` = is recursive with value semantics
  on lists etc. order operations are currently well defined
  only on numbers, but you won't get a type error. all
  comparison functions are variadic and test their arguments
  pairwise left-to-right, with shortcut evaluation when bound
  early.
- there's no built-in `list` but `list = (\ x . x)`
- `*: = car` `:* = cdr` `:: = cons` `*! = set-car!` `!* = set-cdr!`
- `homp` `nump` `twop` `symp` `nilp` type predicates
- hash functions: `tbl tbl-set tbl-get tbl-has tbl-keys tbl-len tbl-del` ; see prelude.lips for usage
- string functions: n-ary constructor `(str 97 97 97) = "aaa"` ; `str-len str-get`

## macros
these are defined in `prelude.lips`, `make repl` imports them automatically
- `(::: nom def)` define macro, like even `:`.
  using it elsewhere than at toplevel is probably a bad idea.
- `(>>= x f) = (f x)` bind, so you can play like it's haskell.
  `(>>= 1 (\ x (>>= 2 (\ y (+ x y)))))`. actually this is pretty
  useful.

## code examples
the compiler is in `prelude.lisp`.

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
(: li (\ x . x) ; list
   quine ((\ q ((ev q) q)) '(\ i (li i (li '` i)))))
; it's ((\ i (li i (li '` i))) '(\ i (li i (li '` i))))
```
## missing features
- exceptions
- basic string functions
- arrays, floats and many other types
- unicode
- useful i/o
- namespaces / module system
