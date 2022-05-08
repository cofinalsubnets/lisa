(: (or_ l r) (? (= l r) l (L 'or l r))
   (and_ l r) (? (= l r) l (L 'and l r))

   (lhs x) (AB x)
   (rhs x) (A (BB x))

   (andp x) (&& (2p x) (= 'and (A x)))
   (orp  x) (&& (2p x) (= 'or  (A x)))
   (notp x) (&& (2p x) (= 'not (A x)))

   (nnf x) (:
    (not_ x) (?
     (notp x) (lhs x)
     (andp x) (or_  (not_ (lhs x)) (not_ (rhs x)))
     (orp  x) (and_ (not_ (lhs x)) (not_ (rhs x)))
              (L 'not x))
    (? (notp x) (not_ (nnf (lhs x)))
       (2p x) (X (A x) (map nnf (B x)))
       x))

   
   ((nf c1 p1 c2 p2) x) (>>= (nnf x) (: (_nf x) (?
    (p1 x) (: l (_nf (lhs x)) r (_nf (rhs x))
     (? (p2 l) (_nf (c2 (c1 (lhs l) r) (c1 (rhs l) r)))
        (p2 r) (_nf (c2 (c1 l (lhs r)) (c1 l (rhs r))))
        (c1 l r)))
    (2p x) (X (A x) (map _nf (B x)))
    x)))



   dnf (nf and_ andp or_ orp)
   cnf (nf or_ orp and_ andp)

   ex1  '(or (not p) (and q (not (and (not r) s))))
   nnf1 '(or (not p) (and q (or r (not s))))
   cnf1 '(and (or (not p) q) (or (not p) (or r (not s))))
   dnf1 '(or (not p) (or (and q r) (and q (not s))))

   ex2  '(or (and q r) (and (not q) (or a b)))
   dnf2 '(or (and q r) (or (and (not q) a) (and (not q) b)))

   ex3 '(and (or a b) (and (or c d) (and (or e f) (or g h))))
   dnf3 '(or (or (or (or (and a (and c (and e g)))
                         (and a (and c (and e h))))
                     (or (and a (and c (and f g)))
                         (and a (and c (and f h)))))
                 (or (or (and a (and d (and e g)))
                         (and a (and d (and e h))))
                     (or (and a (and d (and f g)))
                         (and a (and d (and f h))))))
             (or (or (or (and b (and c (and e g)))
                         (and b (and c (and e h))))
                     (or (and b (and c (and f g)))
                         (and b (and c (and f h)))))
                 (or (or (and b (and d (and e g)))
                         (and b (and d (and e h))))
                     (or (and b (and d (and f g)))
                         (and b (and d (and f h)))))))

   ex4 '(and (or a b) (or c d))
   dnf4 '(or (or (and a c) (and a d)) (or (and b c) (and b d)))

   ex5 '(and (or a b) (and (or c d) (or e f)))
   dnf5 '(or (or (or (and a (and c e))
                     (and a (and c f)))
                 (or (and a (and d e))
                     (and a (and d f))))
             (or (or (and b (and c e))
                     (and b (and c f)))
                 (or (and b (and d e))
                     (and b (and d f))))))

   (test "boolean normal forms"
     (= (nnf ex1) nnf1)
     (= (dnf ex1) dnf1)
     (= (cnf ex1) cnf1)
     (= (dnf ex2) dnf2)
     (= (dnf ex4) dnf4)
     (= (dnf ex5) dnf5)
     (= (dnf ex3) dnf3))
