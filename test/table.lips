(test tables
 (: t (ap tbl (iota 100))
  (, (each (tkeys t) (\ k (? (odd (/ k 2)) (tdel t k))))
     (= (len (tkeys t)) (tlen t) 25)))
)
