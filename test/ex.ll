 ; church numerals
(: ; cf. SKI combinators
   one id ; I
   zero (\ one) ; KI
   ((((add g) f) x) y) ((f x) ((g x) y)) ; ~S ; liftA2 (.) -- in haskell
   (((succ n) f) x) ((n f) (f x))
   ((add g) f) ((f succ) g)
   ((mul g) f) ((f (add g)) zero)
   ((pow g) f) ((f (mul g)) one)
   ((tet g) f) ((f (pow g)) one)
   ((pen g) f) ((f (tet g)) one) ; etc

   (C n) (? (= n 0) zero (succ (C (- n 1)))) ; ℕ->⛪
   (N c) ((c (\ x (+ x 1))) 0)               ; ⛪->ℕ

   two (succ one) four (two two) five (succ four)
   three (succ two)
   ten ((mul two) five)
   one-hundred (two ten))

(: (hy x n y) ( ? (~ n) (+ x y) (~ y) 1
   (hy x (- n 1) (hy x n (- y 1)))))
; some miscellaneous functions
(test execution

 (: (ack m n)
   (? (< m 1) (+ n 1)
    (ack (- m 1) (? (> n 0) (ack m (- n 1)) 1)))
  (= 1021 (ack 3 7)
   ; ack(m > 0, n) = 2[m](n + 3) - 3
   (- (hy 2 2 10) 3)
   (- (N ((pow two) ten)) 3)))
 
 (: (fib n)
   (? (< n 3) 1
    (+ (fib (- n 1)) (fib (- n 2))))
  (= 144 (fib 12)))
 
 (: quine ((\ q (L q (Q q))) '(\ q (L q (Q q))))
  (= quine (ev quine)))
 (: (tarai x y z) (?
     (<= x y) y
     (tarai (tarai (- x 1) y z)
            (tarai (- y 1) x z)
            (tarai (- z 1) x y)))
  (= 13 (tarai 12 13 14)))

 ; hyperoperations
 (: (pow a b) (hy a 2 b)
    (tet a b) (hy a 3 b)
  (= 65536 (<< 1 16) (pow 2 16) (tet 2 4))))
