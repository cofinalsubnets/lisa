# another programming language
a.k.a
- lips
- lisa
- rat
- y(L)

## build / install
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## usage

`#f` = `()` = `0`

| scheme                     | lisa        |
|----------------------------|-------------|
|`(begin a b c)`             |`(, a b c)`  |
|`(lambda - #f)`             |`(\)`        |
|`(lambda - a)`              |`(\ a)`      |
|`(lambda (a) b)`            |`(\ a b)`    |
|`(lambda (a b) c)`          |`(\ a b c)`  |
|`(lambda (a . b) c)`        |`(\ a b . c)`|
|`(begin (define a b) a)`    |`(: a b)`    |
|`(begin (define a b) c)`    |`(: a b c)`  |
|`(cond (a b) (#t #f))`      |`(? a b)`    |
|`(cond (a b) (#t c))`       |`(? a b c)`  |
|`(cond (a b) (c d) (#t #f))`|`(? a b c d)`|

etc.

`.` is a normal symbol with special meaning in `\`. atoms in
tail position are not shown.

some useful functions:
- `+ - * / % & | ^ ~ << >> < <= = >= >`
- `X A B` ~ `cons car cdr`
- `sym` ~ `gensym`
- `.` show args then newline, return last arg or 0
- functions for strings & hash tables are not stable
- other functions defined in `lib`

## examples

### a quine
```lisp
((\ - (L - (L ` -))) '(\ - (L - (L ` -))))
```

### church numerals
```lisp
(:
 ; zero is the constant function at the identity function
 zero (\ (\ - -))
 ; a successor applies its argument then yields to its predecessor
 (((succ f) g) h) ((f g) (g h))

 one (succ zero) ; \ f -> f = identity
 two (succ one) ; \ f -> f . f = square
 three (succ two) ; \ f -> f . f . f = cube 

 ; binary operations follow from succ:
 ((add g) f)         ; the monoid on N
  ((f succ) g)       ; \ g f x -> f x . g x = liftA2 (.)
 ((mul g) f)         ; the monoid on End(N)
  ((f (add g)) zero) ; \ g f x -> f (g x) = (.)

 ; the rest are iterations of the "up arrow" map
 (((up op) g) f) ((f (op g)) one)
 pow (up mul) ; exponentiation ; \ f -> f = id = one
 tet (up pow) ; tetration, etc.

 (C n) (? n (succ (C (- n 1))) zero) ; fixnum -> N
 (N c) ((c (\ x (+ x 1))) 0))        ; N -> fixnum
```

### fizzbuzz
```lisp
(: (/p m n) (~ (% m n))
   (fizzbuzz m)
    ((\ (+ m 1)) (. (?
     (/p m 15) 'fizzbuzz
     (/p m 5)  'buzz
     (/p m 3)  'fizz
     m)))
 ((((pow ((mul ((add three) two)) two)) two) fizzbuzz) 1))
```
