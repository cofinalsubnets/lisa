# lips
lips is a simple lisp variant with a runtime written in C and
a self-hosting threaded code compiler that makes it faster than
most interpreted languages.

## build / install
are you on linux? `make` will probably work. otherwise consult
the makefile for the C compiler invocation. `make install` puts
files under `~/.local/{bin,lib}/`. lips assumes 64-bit word size
but otherwise should be fairly portable.

## syntax
- `()` is self-quoting and false
- there are no improper list literals or quasiquotation
- numbers can take a radix: `0{b,o,d,z,x} -> 2, 8, 10, 12, 16`
- quotes in strings can be escaped with a backslash, but no other
  escape sequences are interpreted; multiline strings are fine

## special forms
all forms take 0 or more arguments. for all forms `f` except `\`:
- `(f) = ()`
- `(f x) = x`
for `\`:
- `(\) = (\ ())`
- `(\ x) = (\ _ x)`

### `,` begin
- `(, a b c) = (begin a b c)`

more useful than in scheme because functions have no implicit
`begin`.

### <code>\`</code> quote
- <code>(\` x) = (quote x)</code>

`'x` works too.

### `?` cond
- `(? a b c) = (cond (a b) (#t c))` if then else
- `(? a b) = (cond (a b) (#t #f))` if then

takes any number of branches. with no fallthrough branch the
implicit value is nil (`()`), which is the only false value
(like in common lisp, hence `#f` in scheme).

### `:` define / letrec
- `(: a0 b0 ... an bn) = (begin (define a b) ... (define an bn) an)` even arguments : define variables in the current scope
- `(: a0 b0 ... an bn c) = (letrec ((a0 b0) ... (an bn)) c)` odd arguments : define variables and evaluate an expression in an inner scope
- `(: ((f g) x y) (g x y)) = (begin (define (f g) (lambda (x y) (g x y))) f)` nestable sugar for function defs

### `\` lambda
- `(\) = (lambda () #f)` nullary -> empty function
- `(\ (f x)) = (lambda () (f x))` unary -> a thunk
- `(\ a0 ... an x) = (lambda (a0 ... an) x)` however many arguments and one expression
- `(\ a b . (a b)) = (lambda (a . b) (a b))`  vararg syntax : `.` after last argument

calling a function with extra arguments is fine but not enough is an error.

## some predefined functions / macros
some of these are defined in prelude.lips, so won't be available unless you bootstrap.

- `L = list` `X = cons` `A = car` `B = cdr`.  `AA`-`BB` are macros.
- `+ - * / % << >> & | ^` like C. ints are currently only 61 bits though
- `< <= >= >` variadic, test each successive pair of arguments, works on numbers.
- `=` variadic, works on anything, recursive on pairs so `(= (L 1 2 3) (L 1 2 3))`.
- `ev ap ccc` = `eval apply call/cc`
- `cu` partial apply : `((cu f a b) c d) = (f a b c d)` `co` compose : `((co f g h) x) = (h (g (f x)))`
- `.` print arguments separated by spaces, print newline, return last argument
- `iota` and `rho` are kind of like in APL but not as good
- `homp nump twop symp nilp tblp strp vecp` type predicates
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
(: (hy n) (? (= n 0) +
 (\ x y (foldr1 (rho y x) (hy (- n 1))))))
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

### μKanren
```lisp
(: (_? x) (&& (symp x) (~ (ystr x))) ; var?

   ((disj f g) x) (m+ (f x) (g x))
   ((conj f g) x) (m* (f x)  g)

   ; mplus/bind
   (m+ a b) (? (~ a)  b (homp a) (\            (m+    b    (a)))
                                 (X     (A a)  (m+    b  (B a))))
   (m* a b) (? (~ a) () (homp a) (\            (m*   (a)    b))
                                 (m+ (b (A a)) (m* (B a)    b)))

   == (: (walk u s) (|| (&& (_? u)
                            (: r (find s (co B (cu = u))))
                            (walk (A r) s))
                        u)
         ((unify u v) s) (: x (walk u s) y (walk v s) (?
          (= x y) s             ; ok
          (_? x) (X (X y x) s)  ; x <- y
          (_? y) (X (X x y) s)  ; y <- x
          (&& (twop x) (twop y) ; unify both sides
           (unify (B x) (B y) (unify (A x) (A y) s)))))
       (co unify (\ _ (co _ L)))))

; call/fresh ;; it's a macro, sorry
(::: \\ (\ a . (X (X '\ a) (map (init a) (\ (L sym))))))

(. ((\\ a b (conj (== a 5) (disj (== b 6) (== b 7)))) ()))
```

## missing features
### general purpose functionality
unicode, floats, arrays, files, networking, ...

### type inference
like typescript but less fascist.

### polymorphism / overloading
of functions like `+`, etc. type system will help.

### namespace / module system
for ease of use and has practical benefits for compiling.
