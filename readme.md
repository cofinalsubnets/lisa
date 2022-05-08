# ll
lisp

## build / install
are you on linux? `make` will probably work. otherwise, see
`Makefile` for the suggested compiler flags.

## syntax
- `()` = `0` and is uniquely `#f`
- numbers may take a C-style radix in `{b=2,o=8,d=10,z=12,x=16}`
- strings delimited by `"` which can be escaped with a backslash

## special forms

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
