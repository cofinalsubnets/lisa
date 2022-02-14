# lips
nonverbal lisp

## build / install
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## syntax quick facts
- `()` is self-quoting and false
- there are no improper list literals or quasiquotation
- numbers can take a radix: `0{b,o,d,z,x} -> 2, 8, 10, 12, 16`
- quotes in strings can be escaped with a backslash, but no other
  escape sequences are interpreted; multiline strings are fine

## special forms
|  lips       | scheme           |
|-------------|------------------|
|`(, a b)`    |`(begin a b)`     |
|`(\)`        |`(lambda _ #f)`   |
|`(\ x)`      |`(lambda _ x)`    |
|`(\ a b c)`  |`(lambda (a b) c)`|
|`(: a b)`    |`(begin (define a b) a)`|
|`(: a b c)`  |`(letrec ((a b)) c)`|
|`(? a b)`    |`(cond (a b) (#t #f))`|
|`(? a b c)`  |`(cond (a b) (#t c))`|
|`(? a b c d)`|`(cond (a b) (c d) (#t #f))`|
|`'x` <code>(\` x)</code>|`'x` `(quote x)`  |

etc.

## code examples

### hyperoperations

```lisp
; send n to the nth hyperoperation where 0 is +
(: (hy n) (? (= n 0) +
 (\ x y (foldr1 (rho y x) (hy (- n 1))))))
```

### a quine

```lisp
((\ - (L - (L ` -))) '(\ - (L - (L ` -))))
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

## some predefined functions / macros
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
- symbol functions: `sym`: with no argument, `gensym`; with an argument, `string->symbol`
- `(::: nom0 def0 nom1 def1 ...)` define macro
- `(>>= x y z f) = (f x y z)` is sometimes preferable for
  readability

## missing features

### general purpose functionality
- floats
- arrays
- module system
- file system
- network

### parser
- reader macros

### compiler
- type inference
- odd `:` should behave like `let*` not `letrec`
- variable renaming to minimize closure creation
