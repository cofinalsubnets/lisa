(: (memo1 f) (: mem (tbl) (\ x (? (thas mem x) (tget mem x) (tset mem x (f x)))))
   (memo* f) (: mem (tbl) (\ x . (? (thas mem x) (tget mem x) (tset mem x (ap f x)))))
)

(: box (tbl () 0)
   (incr) (tset box () (+ 1 (tget box ())))
   (reset) (tset box () 0)
   (count) (tget box ())
   F (memo1 (\ x (, (incr) x)))
   G (memo* (\ x y z (, (incr) (* z (+ x y)))))
   (test memoization
    (, (F 1) (F 1) (G 1 2 3) (G 1 2 3)
       (= 2 (count)))))
