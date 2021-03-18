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
  `(= (li 1 2 3) (li 1 2 3)).
- `ev = eval`, `ap = apply`, `ccc = call/cc`
- `.` print arguments separated by spaces, print newline, return
  last argument; useful for debugging.
- `&&` `||` left-to-right, shortcut eval when bound early.
- there's no built-in `list` but `list = (\ x . x)`
- `*: = car` `:* = cdr` `:: = cons` `*! = set-car!` `!* = set-cdr!`
- `homp` `nump` `twop` `symp` `nilp` type predicates
- hash functions: `tbl tbl-set tbl-get tbl-has tbl-keys tbl-len tbl-del` ; see prelude.lips for usage
- string functions: n-ary constructor `(str 97 97 97) = "aaa"` ; `str-len str-get`

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
((\ i (li i (li '` i))) '(\ i (li i (li '` i))))
```
## missing features
- exceptions
- arrays, floats and many other types
- unicode
- useful i/o
- namespaces / module system
