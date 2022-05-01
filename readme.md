# empath
limit language

## build / install
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## syntax fax
- `()` is false & fixed by `ev`
- no improper list literals or quasiquotation
- numbers may take a C-style radix in `{b=2,o=8,d=10,z=12,x=16}`
- strings delimited by `"` which can be escaped with a backslash;
  no other escape sequences are interpreted

## special forms

|  lips       | scheme                     |
|-------------|----------------------------|
|`(, a b)`    |`(begin a b)`               |
|`(\)`        |`(lambda _ #f)`             |
|`(\ x)`      |`(lambda _ x)`              |
|`(\ a b c)`  |`(lambda (a b) c)`          |
|`(: a b)`    |`(begin (define a b) a)`    |
|`(: a b c)`  |`(letrec ((a b)) c)`        |
|`(? a b)`    |`(cond (a b) (#t #f))`      |
|`(? a b c)`  |`(cond (a b) (#t c))`       |
|`(? a b c d)`|`(cond (a b) (c d) (#t #f))`|
|`'x`         |`'x`                        |

etc.

## code examples

### a quine
```lisp
((\ - (L - (L ` -))) '(\ - (L - (L ` -))))
```

### hyperoperations
```lisp
; send n to the nth hyperoperation where 0 is +
(: (hy n) (? (= n 0) +
 (\ x y (foldr1 (rho y x) (hy (- n 1))))))
```

### church numerals
```lisp
(: (((mul f) g) x) (f (g x))
   ((((add f) g) x) y) ((f x) ((g x) y))
   one (\ - -)
   zero (\ one)

   (C n) (? (= n 0) zero ((add one) (C (- n 1)))) ; ℕ->⛪
   (N c) ((c (\ x (+ x 1))) 0))                   ; ⛪->ℕ

(: (/p m n) (= 0 (% m n))
   (fizzbuzz m)
    ((\ (+ m 1)) (. (?
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
- `(>>= x y z f) = (f x y z)` is sometimes preferable for readability
