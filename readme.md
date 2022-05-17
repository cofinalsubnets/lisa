# lisa
LISt Action

## build / install
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## syntax

lisa is currently highly unstable.
special forms compared to scheme:

| scheme                     |             |
|----------------------------|-------------|
|`(begin a b)`               |`(, a b)`    |
|`(lambda _ #f)`             |`(\)`        |
|`(lambda _ x)`              |`(\ x)`      |
|`(lambda (a b) c)`          |`(\ a b c)`  |
|`(begin (define a b) a)`    |`(: a b)`    |
|`(letrec ((a b)) c)`        |`(: a b c)`  |
|`(cond (a b) (#t #f))`      |`(? a b)`    |
|`(cond (a b) (#t c))`       |`(? a b c)`  |
|`(cond (a b) (c d) (#t #f))`|`(? a b c d)`|
|`'x`                        |`'x`         |

etc.

other things:

- `()` = `#f` = `0`
- numbers may take a C-style radix in `{b=2,o=8,d=10,z=12,x=16}`
- strings delimited by `"` which can be escaped with a backslash

## code examples

### a quine
```lisp
((\ - (L - (L ` -))) '(\ - (L - (L ` -))))
```

### hyperoperation sequence
```lisp
(: (hy x n y) ( ? (~ n) (+ x y) (~ y) 1
 (hy x (- n 1) (hy x n (- y 1)))))
```

### church numerals
```lisp
(:
 ; church numerals are an encoding of natural numbers as
 ; elements of a monoid of functions N = 0 + succ N. the
 ; hyperoperation sequence then appears as N different
 ; "complexity classes" of self-actions of N.

 ; zero is the constant function at the identity
 zero (\ (\ - -))
 ; a successor applies f then yields to its predecessor
 (((succ p) f) -) ((p f) (f -))

 ; here are three more numbers
 one (succ zero) ; \ f -> f = identity
 two (succ one) ; \ f -> f . f = square
 three (succ two) ; \ f -> f . f . f = cube 

 ; other operations follow from succ
 ; the monoid operation on N
 ((add g) f) ((f succ) g) ; \ g f x -> f x . g x
 ; the monoid operation on End(N)
 ((mul g) f) ((f (add g)) zero) ; \ g f x -> f (g x)

 ; the rest are iterations of the "up arrow" map
 (((up op) g) f) ((f (op g)) one)
 pow (up mul) ; exponentiation
 tet (up pow) ; tetration, etc.

 ; it's interesting to note that under this scheme
 ;   (a b) = ((pow b) a)
 ; in other words, the (flipped) exponentiation operator
 ; is extensionally equivalent to the identity function,
 ; which also happens to define the number one.

 (C n) (? (= n 0) zero (succ (C (- n 1)))) ; ℕ->⛪
 (N c) ((c (\ x (+ x 1))) 0))              ; ⛪->ℕ

(: (/p m n) (= 0 (% m n))
   (fizzbuzz m)
    ((\ (+ m 1)) (. (?
     (/p m 15) 'fizzbuzz
     (/p m 5)  'buzz
     (/p m 3)  'fizz
     m)))
 ((((pow ((mul ((add three) two)) two)) two) fizzbuzz) 1))
```
