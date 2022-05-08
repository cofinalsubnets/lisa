; matrix arithmetic
(: m+ (cu zip (cu zip +))
   (m* a b)
    (: bT (transp b)
     (map (\ row (map (\ col
      (foldr (zip * row col) 0 +)) bT)) a))
   m0 '((1 2 3) (4 5 6) (7 8 9))
   m1 '((1 0 0) (0 1 0) (0 0 1))
   m2 '((2 0 0) (0 2 0) (0 0 2))
   m3 '((30 36 42) (66 81 96) (102 126 150)))

(test math
 (= 144 (* 12 12) (* 2 2 3 2 2 3))
 (= -99 (- 1 49 51) (- 99))
 (= 1024 (* (* (* 2 2 2) 2 2) (* 2 2 (* 2 2 2))) (<< 1 10))
 (= 1040 (| (<< 1 10) (<< 1 2 2)))
 (= 3 (>> (& 15 6 7) 1) (% 25 9 4))
 (= 4 (/ 16 2 2) (/ 16 4) (>> 16 2) (<< 1 2))
 (= 1 (% 11 6 2) (& 1 11) (- 100 50 49))
 (< 1 (<< 1 1) (<< 1 1 1) (<< 1 1 1 1))
 (~ (< 1 (<< 1 1) (<< 1 1) (<< 1 1 1)))
 (>= 30 29 29 27 27 4 0 0)
 (= m3 (m* m0 m0))
 (= m0 (m* m0 m1))
 (= m1 (m* m1 m1))
 (= m2 (m+ m1 m1))
 (= (m+ m0 m0) (m* m0 m2)))