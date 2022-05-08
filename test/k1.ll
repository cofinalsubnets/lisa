(::: zz (\ x (: a (sym) b (sym) c (sym) (L \ a (L \ b c (L c (L x a))))))
 et (\ x xs . (foldl xs x (\ a b (L (\ a b (\ s (s* (a s) b))) a b))))
 vel (\ x xs . (foldl xs x (\ a b (L (\ a b (\ s (s+ (a s) (b s)))) a b)))))
(:
 ((sX x s) _ _ y) (y x s)
 (sno n) (n)
 (sun x) (sX x sno)

 (s+ s t) (s (\ t)
             (\ s (\ _ m (m (s+ t s))))
             (\ x s (sX x (s+ s t))))

 (s* s g) (s (\ s)
             (\ s (\ _ m (m (s* s g))))
             (\ x s (s+ (g x) (s* s g))))

 (stake i s)
  (? (= i 0) sno
   (s (\ s)
      (\ s (stake i s))
      (\ x s (\ _ _ y (y x (stake (- i 1) s))))))

 (smap f s)
  (s (\ s)
     (\ s (\ _ m (m (smap f s))))
     (\ x s (sX (f x) (smap f s))))

 (sfold f z s)
  (s (\ z)
     (\ s (sfold f z s))
     (\ x s (f x (sfold f z s))))

 (slist s) (sfold X () s)

 ((est u v) s) (:
  (walk u s) (?  (&& (var? u) (: t (has-s s u))) (walk (A t) s) u)
  x (walk u s) y (walk v s) (?
   (= x y)  (sun s)
   (var? x) (sun (ext-s s x y))
   (var? y) (sun (ext-s s y x))
   (&& (2? x) (2? y)) ((et (est (A x) (A y)) (est (B x) (B y))) s)
   sno))

 (Run n ks g) (:
  (walk* u s) (?
   (var? u) (? (: t (has-s s u)) (walk* (A t) s) u)
   (2? u) (X (walk* (A u) s) (walk* (B u) s))
   u)
  (re st) (flat-map ks (\ k
   (? (: it (has-s (X (map A (A st)) (B st)) k)) (L k (walk* (A it) st)))))
  (slist (smap re (? n (stake n (g ~~)) (g ~~))))))

(test k1
 (unord-eq '((a (1 2 (3 (4))) b (3 (4)) c 4)) (Run () '(a b c)
  (\\ a b c (et (est a (L 1 2 b))
                (est b (L 3 (L c)))
                (est c 4)))))
 (= (rho 9 '(a 9)) (Run 9 '(a)
  (\\ a (: nines (vel (est a 9) (zz nines))))))
 (unord-eq (map (\ i (L 'a i)) (iota 10)) (Run 10 '(a)
  (\\ a (:
   (at-least n) (vel (est a n) (zz (at-least (+ n 1))))
   (vel (\ sno) (est a 0) (at-least 1))))))
 (unord-eq '((a 7 b 6) (a 7 b 5)) (Run () '(a b)
  (\\ a b (et (est a 7) (vel (est b 5) (est b 6))))))
 (unord-eq '((a 9 b 8)) (Run () '(a b)
  (\\ a b (et (est a 9) (est b 8)))))
 (unord-eq '((a 1 b 1)) (Run () '(a b)
  (\\ a b (et (est a 1) (est a b)))))
 (unord-eq '((a 7 b 3)) (Run () '(a b)
  (\\ a b (et (est a 7)
              (est b 3)
              (vel (est b 3) (est b 4))))))
 (unord-eq '((a (1 2) b 2)) (Run () '(a b)
  (\\ a b (et (est a (L 1 b)) (est b 2)))))
 (~ (Run () '(a) (\\ a (et (est a 0) (est a 1)))))
 (~ (Run () '(a b) (\\ a b
  (et (est a 7) (est b 3) (vel (est b 2) (est b 4))))))
 (unord-eq '((a 1 b 2 c 2) (b 9 c 7) (c 88))
  (Run () '(a b c) (\\ a b c (vel
   (et (est a 1) (est b 2) (est c b))
   (et (est c 7) (est b 9))
   (est c 88)))))
 (unord-eq '((a 7 b 8 c 9) (a 4 b 5 c 6) (a 1 b 2 c 3))
  (Run () '(a b c) (\\ a b c (vel
   (et (est a 1) (est b 2) (est c 3))
   (et (est a 4) (est b 5) (est c 6))
   (et (est a 7) (est b 8) (est c 9))))))
 (unord-eq (map (\ i (L 'a i)) (iota 10))
  (Run 10 '(a) (\\ a
   (: never (vel (\ sno) (zz never))
      (at-least n) (vel (est a n) (zz (at-least (+ n 1))))
    (vel never (est a 0) (at-least 1))))))
 (: inc L
    (iter n f x) (? (= 0 n) x (iter (- n 1) f (f x)))
    zero () one '(()) two '((()))
    (add a b c) (vel (et (est a ()) (est b c))
                     (\\ x (et (est (inc x) a)
                               (zz (add x (inc b) c)))))
    (fibo a b) (vel (et (est a one) (est b one))
                    (et (est a two) (est b one))
                    (\\ x y p q
                     (et (est (inc x) a)
                         (est (inc y) x)
                         (zz (et (fibo x p)
                                 (fibo y q)
                                 (add p q b))))))
    (= (L (L 'a (iter 144 L ())))
     (Run () '(a) (\\ a (fibo (iter 12 L ()) a))))))
