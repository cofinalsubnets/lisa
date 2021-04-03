# lips
nonverbal lisp

## lisp qualities
- strict lexically scoped lisp-1 with short function words
  and no superfluous parentheses
- annotation-free hybrid static/dynamic typing
- lists and definitions (except at toplevel) are immutable
- `()` is self-quoting and false
- `.` is a regular symbol

## build / install
are you on linux? `make` should work. otherwise consult the
makefile for the C compiler invocation. `make install` puts
the binary and prelude under `~/.local` by default.

## special forms
with scheme equivalents. degenerate (nullary/unary) cases
are usually nil or the identity; `\`is an exception to this

### `,` begin
- `(, a b c) = (begin a b c)`

### <code>\`</code> quote
- <code>(\` x) = (quote x)</code>
`'x` also works.

### `?` cond
- `(? a b) = (cond (a b) (#t '()))`
- `(? a b c) = (cond (a b) (#t c))`
- `(? a b c d) = (cond (a b) (c d) (#t '()))`

### `:` define / let
- `(: a b) = (begin (define a b) a)`
- `(: a b c d) = (begin (define a b) (define c d) c)`
- `(: a b c d e) = (letrec ((a b) (c d)) e)`
- `(: (a b c) (b c)) = (define (a b c) (b c))`

### `\` lambda
- `(\) = (lambda () ())`
- `(\ a) = (lambda () a)`
- `(\ a a) = (lambda (a) a)`
- `(\ a b (a b)) = (lambda (a b) (a b))`
- `(\ a b . (a b)) = (lambda (a . b) (a b))`
- `(\ a b c (, x y z)) = (lambda (a b c) x y z)`

nil is the only false value


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
  last argument; great for debugging.
- `A = car` `B = cdr` `X = cons` `L = list`. `AA`-`BB` are
  defined as macros.
- apl-like data constructors: `iota` is monadic `ι`; `rho` is
  a weaker dyadic `ρ`: `(ap rho (X n l))` concatenates n copies
  of l.
- `homp` `nump` `twop` `symp` `nilp` `tblp` `strp` `vecp` type predicates
- hash functions: `tbl tset tget thas tkeys tlen tdel` ; see prelude.lips for usage
- string functions: n-ary constructor `(str 97 97 97) = "aaa"` ; `slen sget ssub scat`
- symbol functions: `gensym`

## macros
these are defined in `prelude.lips`, `make repl` imports them automatically
- `(::: nom0 def0 nom1 def1 ...)` define macro
- `(>>= x y z f) = (f x y z)`

## fun code examples

### a quine
```lisp
((\ i (L i (L '` i))) '(\ i (L i (L '` i))))
```

### hyperoperations
```lisp
; send n to the nth hyperoperation, 0 being addition.
(: hy (\ n (? (= n 0) + (\ x y
 (foldr1 (rho y x) (hy (- n 1)))))))
```

### church numerals
```lisp
; modified SK combinators
(: K const
   I id
   P (\ f (\ g (\ x (f (g x))))) ; just postcomposition
   Q (\ m (\ n (\ f              ; Q : S :: compose : apply
    ((P (m f)) (n f)))))

   zero (K I)
   one I
   exp I
   mul P
   add Q
   succ (add one)

   C (\ n (? (= n 0) zero (succ (C (- n 1))))) ; go to church
   N (\ c ((c (\ x (+ x 1))) 0)))              ; leave
```

### fizzbuzz
```lisp
; now that we have church numerals we can write fizzbuzz.
(: /p (\ m n (= 0 (% m n)))
   fb (\ m (, (. (? (/p m 15) 'fizzbuzz
                    (/p m 5)  'buzz
                    (/p m 3)  'fizz
                    m))
              (+ m 1)))
 (((C 100) fb) 1))
```

## missing features
- arrays, floats and many other types
- unicode
- useful i/o
- namespaces / module system

## further reading / inspiration
- [SICP](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html)
- [Lambda: The Ultimate Imperative](https://dspace.mit.edu/handle/1721.1/5790)
- [Partial Computation of Programs](https://repository.kulib.kyoto-u.ac.jp/dspace/handle/2433/103401)
- [Physics, Topology, Logic and Computation: A Rosetta Stone](https://math.ucr.edu/home/baez/rosetta.pdf)
