; heron's method for finding square roots
; named for hero(n) of alexandria (ca 10-70)
; fun fact: always converges to root from above (unless initial guess was correct)
(test heron
 (: (heron x g) (? (= g (/ x g)) g
     (heron x (/ (+ g (/ x g)) 2))))
  (= 6 (heron 36 47032)))

(:
 ; church numerals
 ; named for alonzo church (1903-1995)
 ; inventor of lambda notation.
 ; richard dedekind (1831-1916; 1888 ¶126) uses a similar
 ; idea to describe how natural numbers give rise to monoids
 ; of endomorphisms ("chains" of images)
 ((zero) -) -
 one (zero zero)
 (((up op i) g) f) ((f (op g)) i)
 (((succ g) x) y) ((g x) (x y))
 (((ps f) g) h) (h (g f))
 (((pred f) g) h) (((f (ps g)) (\ h)) one)
 ((add g) f) ((f succ) g)
 mul (up add zero)
 pow (up mul one)
 tet (up pow one)
 pen (up tet one)

 (C n) (? (= n 0) zero (succ (C (- n 1)))) ; ℕ->⛪
 (N c) ((c (\ x (+ x 1))) 0)               ; ⛪->ℕ

 two (succ one) four (two two) five (succ four)
 three (succ two)
 ten ((mul two) five)
 one-hundred (two ten)

 ; fixnum hyperoperation sequence, rooted at +
 (hy x n y) (? (= 0 n) (+ x y) (= 1 n) (* x y) (nilp y) 1
   (hy x (- n 1) (hy x n (- y 1)))))

; tarai function of ikuo takeuchi (1978)
; "for comparing the speeds of LISP systems ... without
; generating large numbers or using much stack"
; (mccarthy 1978)
(test takeuchi
 (: (tarai x y z) (?
     (<= x y) y
     (tarai (tarai (- x 1) y z)
            (tarai (- y 1) x z)
            (tarai (- z 1) x y)))
  (= 13 (tarai 12 13 14))))

(test ackermann
 ; non-primitive-recursive function of wilhelm ackermann
 ; (1896-1962)
 (: (ack m n)
   (? (< m 1) (+ n 1)
    (ack (- m 1) (? (> n 0) (ack m (- n 1)) 1)))
  (= 1021 (ack 3 7)
   ; ack(m > 0, n) = 2[m](n + 3) - 3
   (- (hy 2 2 10) 3)
   (- (N ((pow two) ten)) 3))))

(test fibonacci
 ; natural sequence named for leonardo bigollo pisano aka.
 ; fibonacci (ca. 1170-1245)
 (: (fib n)
   (? (< n 3) 1
    (+ (fib (- n 1)) (fib (- n 2))))
  (= 144 (fib 12))))
 
(test cantor (:
 ; cantor's pairing function reversibly encodes a
 ; pair of natural numbers as a natural number.
 (c1895 m n) (+ m (/ (* (+ m n) (+ m n 1)) 2))
 ; according to stillwell (2018) this is the only known
 ; quadratic pairing function & was proved in 1923 by
 ; polya & fueter to be (with its mirror) the only
 ; quadratic pairing function sending (0, 0) to 0.
 ; here's an inverse:
 (c1895~ p) (:
  (f t n) (? (> (+ t n) p)
           (X t (- n 1))
           (f (+ t n) (+ n 1)))
  x (f 0 1)
  m (- p (A x))
  n (- (B x) m)
  (L m n))

 (: 0to23 (iota 24)
    pairs (flat-map 0to23 (\ n (map 0to23 (\ m (L m n)))))
  (all pairs (\ p
   (= p (c1895~ (ap c1895 p))))))))

; hyperoperation sequence

(: (pow a b) (hy a 2 b)
   (tet a b) (hy a 3 b)
 (test hy
  (= 65536 (<< 1 16) (pow 2 16) (tet 2 4))))

; nontrivial fixed points of eval named for willard van
; orman quine (1908-2000)
(test quine
 (: quine ((\ q (L q (Q q))) '(\ q (L q (Q q))))
  (= quine (ev quine))))

(test church
 (= 100
  (N one-hundred)
  (+ 1 (N (pred one-hundred))))
 (= 252
  (N ((add (five three)) (two three)))
  (N ((mul (two three))
      ((mul (two two))
       (pred (pred (two three))))))))
