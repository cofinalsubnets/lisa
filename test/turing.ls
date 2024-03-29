(:
 (turing prog st l h r) (:
  (step prog st l h r)
   (? (: e (find prog (\ e (&& (= (A e) st) (= (AB e) h)))))
      (ap rule (BB e)))
  (rule new-symbol go-right? new-state)
   (X new-state (? go-right?
    (append (L (X new-symbol l))
            (? r (L (A r) (B r)) '(0 0)))
    (append (? l (L (B l) (A l)) '(0 0))
            (L (X new-symbol r)))))
  (trim t) (:
   (t0 t) (? (nilp t) t (nilp (A t)) (t0 (B t)) (t1 t))
   (t1 t) (? (nilp t) t (all t nilp) 0 (X (A t) (t1 (B t))))
   (t0 t))
  (: res (step prog st l h r) (? res
   (ap turing (X prog res))
   (trim (append (rev l) (X h r)))))))

(test "turing machines"
  (= '(2 2 2 2 1)
   (turing
    '((0 1 2 1 0)
      (0 0 1 0 1)
      (1 2 2 0 1)
      (1 0 2 1 halt))
    0 ; initial state 0
    0 ; nothing to the left
    1 ; 1 under the head
    '(1 1) ; two ones to the right
    ))
  (= '(h e l l o w o r l d)
   (turing
    '((b j h 0 halt)
      (a e e 1 a)
      (a l l 1 a)
      (a o o 1 a)
      (a w w 1 a)
      (a r r 1 a)
      (a q d 0 b)
      (b e e 0 b)
      (b l l 0 b)
      (b o o 0 b)
      (b w w 0 b)
      (b r r 0 b))
    'a
    '(o l l e j)
    'w
    '(o r l q))))
