(: 0? (= 0)
   inc (+ 1)
   dec (+ -1))
(assert
 (= 17 ((\ f (f 9)) (+ 8)))
 (= 13 (: a 9 b 7 c -3 (+ a (+ b c))))
 (: (even? n) (? (0? n) -1 (odd? (dec n)))
    (odd? n) (? (0? n) 0 (even? (dec n)))
  (odd? 99)))
