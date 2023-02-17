(:
 ((diag f) x) (f x x)
 I id K const
 (flat-map x f) (? (twop x)
  (append (f (A x)) (flat-map (B x) f)))

 (assq k x) (? x (? (= (AA x) k) (A x) (assq k (B x))))
 (rev l) (foldl 0 l (\ a b (X b a)))
 (find l p) (? l (? (p (A l)) (A l) (find (B l) p)))
 (transp x) (ap zip (X L x))
 (sample l) (at l (randn (len l)))
 (intercal i l) (:
  (loop i l m)
   (? m (append l (append i (intercal i m))) l)
  (? l (loop i (A l) (B l))))
 (even x) (? (= 0 (& x 1)))
 (odd x) (? (= 1 (& x 1)))
 (abs x) (? (< x 0) (- x) x)
 (randn n) (% (abs (rand)) n)
 ; strings
 (chars s) (>>= s 0 (slen s) (: (loop s n l)
  (? (< n l) (X (schr s n) (loop s (+ n 1) l)))))
 (puts s) (each (chars s) putc)
 (iota n x .)
  (: (loop m n) (? (< m n) (X m (loop (+ m 1) n)))
   (? (nilp x) (loop 0 n) (loop n (A x))))
 (rho n xs .)
  (: (loop n x) (? (= n 1) x (append x (loop (- n 1) x)))
   (? (> n 0) (loop n xs))))
