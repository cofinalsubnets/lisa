;(. 2 'eq)
(: (eq-mtx x a b)
    (= x (= a b)
     ((\ x (= a x x x)) b)
     ((\ x (= x a x x)) b)
     ((\ x (= x x a x)) b)
     ((\ x (= x x x a)) b))
 (test equality
  (eq-mtx (nilp 0) 1 1)
  (eq-mtx () 1 0)
  (= (L 0) (L 0))
  (= (L 0 0) (X 0 (X 0 0)))
  (= 'a 'a)
  (= "a" "a")
  (nilp (= "a" "b"))
  (nilp (= (sym) (sym)))
  (nilp (= (tbl) (tbl)))))
