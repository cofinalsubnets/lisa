(test tables
 (: t (ap tbl (iota 100))
  (, (each (tkeys t) (\ k (? (odd (/ k 2)) (tdel t k))))
     (= (len (tkeys t)) (tlen t) 25)))

 (: ; memoization
  (memo1 f) (: mem (tbl) (\ x (? (thas mem x) (mem x) (tset mem x (f x)))))
  (memo* f) (: mem (tbl) (\ x . (? (thas mem x) (mem x) (tset mem x (ap f x)))))
  box (tbl () 0)
  (incr) (tset box 0 (+ 1 (box 0)))
  (reset) (tset box 0 0)
  (count) (box 0)
  F (memo1 (\ x (, (incr) x)))
  G (memo* (\ x y z (, (incr) (* z (+ x y)))))
  (, (F 1)
     (F 1)
     (G 1 2 3)
     (G 1 2 3)
     (= 2 (count)))))
