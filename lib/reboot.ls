; experimental new compiler stuff
(:
 (dict-get d k) (? d
  (? (= k (A d)) (AB d)
   (dict-get (BB d) k)))
 (dict-put d k v) (X k (X v d))
 (check k1 k2) (?
  (= 'val (A k1)) (check-val (B k1) k2)
  (= 'val (A k2)) (check-val (B k2) k1)
  k1
 )
 em poke
 (ini n e l) (l e (em i-ret (hom (+ n 1))))
 (ana-imm n e l x r)
  (r (+ n 2) e (\ e k (l e (em i-imm (em x k)))))
 (ana-sym-upd e) e ; TODO
 (ana-sym n e l x r)
  (r (+ n 2) (ana-sym-upd e x) (\ e k
   (l e (ana-sym-rslv e x k))))
 (ana-begin n e l x r) (?
  (~ x) (ana-imm n e l 0 r)
  (~ (B x)) (ana n e l (A x) r)
  (ana n e l (A x) (\ n e l
   (ana-begin n e l (B x) r))))
 (ana n e l x r)
  ((? (twop x) ana-two (symp x) ana-sym ana-imm) n e l x r)
 (ana-two n e l x r) (: a (A x) (?
  (= '` a) (ana-imm n e l (? (B x) (AB x)) l r)
  (= ', a) (ana-begin n e l (B x) r)
  ; TODO
  (= ': a) (ana-define n e l (B x) r)
  (= '\ a) (ana-lambda n e l (B x) r)
  (= '? a) (ana-cond n e l (B x) r)
  (ana-funcall n e l x r)))
 (end e k) (hfin k)
 )
