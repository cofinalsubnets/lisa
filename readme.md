# lips
## special forms
with scheme equivalents
### `,` begin
- `(, a b c) = (begin a b c)`

### `:` define / let
- `(: a b) = (begin (define a b) a)`
- `(: a b c d) = (begin (define a b) (define c d) c)`
- `(: a b c d e) = (let* ((a b) (c d)) e)`

### `\` lambda
- `(\ a) = (lambda () a)`
- `(\ a a) = (lambda (a) a)`
- `(\ a b (a b)) = (lambda (a b) (a b))`
- `(\ a b . (a b)) = (lambda (a . b) (a b))`
- `(\ a b c (, x y z)) = (lambda (a b c) x y z)`

### `?` cond
- `(? a b) = (cond (a b) (#t '()))`
- `(? a b c) = (cond (a b) (#t c))`
- `(? a b c d) = (cond (a b) (c d) (#t '()))`

#### <code>\`</code> quote
- <code>(\` x) = (quote x)</code>

### functions
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
- hash functions: nullary constructor `tbl`; `tbl-set tbl-get tbl-has tbl-keys tbl-len tbl-del`
- string functions: n-ary constructor `(str 97 97 97) = "aaa"` ; `str-len str-get`

### code examples
#### fizzbuzz
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

#### a quine
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
