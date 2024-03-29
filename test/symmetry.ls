; modeling symmetry groups
; a permutation p on n elements is represented as
; a permutation of (iota n) sending the element
; at index i to index p[i]. composition then
; coincides with application.
(:
 id-perm iota
 ; apply a permutation to a representative
 (ap-perm p q)
  (? p (X (at q (A p)) (ap-perm (B p) q)))
 co-perm (flip ap-perm)
 (permutations n) (:
  (seps n) (:
   (se l n r)
    (X (X n (append l r)) (? r (se (X n l) (A r) (B r))))
   (se 0 (A n) (B n)))
  (? (nilp n) '(0)
   (flat-map (seps n) (\ l
    (map (permutations (B l)) (\ z (X (A l) z)))))))
 (cy2pm n c) (: a (A c)
  (cy2pm_ c) (? (B c) (X (L (A c) (AB c)) (cy2pm_ (B c)))
                (L (L (A c) a)))
  q (cy2pm_ c)
  (map (iota n) (\ i (: x (assq i q) (? x (AB x) i))))))

(test permutations
 ; associativity
 (: perms (permutations (id-perm 4))
  (all perms (\ p0
   (all perms (\ p1
    (all perms (\ p2
     (= (ap-perm p0 (ap-perm p1 p2))
        (ap-perm (ap-perm p0 p1) p2)))))))))
 (= '(4 2 1 3 0)
  (ap-perm
   (cy2pm 5 '(0 4))
   (cy2pm 5 '(1 2)))))

(: cy3 (cu cy2pm 3))
(test s3
 ; identity
 (= '(0 1 2) (cy3 '(0)) (cy3 '(1)) (cy3 '(2)))
 ; 3 2-cycles
 (= '(0 2 1) (cy3 '(1 2)) (cy3 '(2 1)))
 (= '(1 0 2) (cy3 '(0 1)) (cy3 '(1 0)))
 (= '(2 1 0) (cy3 '(0 2)) (cy3 '(2 0)))
 ; 2 3-cycles
 (= '(1 2 0) (co-perm (cy3 '(0 2))
                      (cy3 '(0 1))))
 (= '(2 0 1) (co-perm (cy3 '(1 2))
                      (cy3 '(0 1))))

 ; (2 1 0)(0 1) = (0 2)(2 1 0) = (1 2)
 (= (co-perm (cy3 '(2 1 0))
             (cy3 '(0 1)))
    (co-perm (cy3 '(0 2))
             (cy3 '(2 1 0)))))
