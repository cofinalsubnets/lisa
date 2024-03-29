; microKanren without reflection on streams
(:::
 \\ (\ a . (X (X '\ a) (map (init a) (\ a (L var (Q a))))))
 zzz (\ x (: y (sym) (L '\ y  (L '\ (L x y))))))
(:
 ; dict - immutable map type made of pairs
 (dict-has d k) (:
  (loop k ks vs) (? ks
   (? (= k (A ks)) vs (loop k (B ks) (B vs))))
  (loop k (A d) (B d)))
 (dict-get d k) (? (: x (dict-has d k)) (A x))
 (dict-set d k v) (X (X k (A d)) (X v (B d)))

 (anon? x) (&& (symp x) (nilp (ynom x)))
 (var? x) (&& (twop x) (anon? (B x)))
 (var x) (X x (sym))

  has-s dict-has
  (ext-s s k v) (dict-set s k v)
 (unord-eq a b) (&&
  (= (len a) (len b))
  (all b (cu memq a)))

 )
(:::
 zz (\ x (: a (sym) b (sym) c (sym)
  (L '\ a (L '\ b c (L c (L x a))))))
 et (\ x xs . (foldl x xs (\ a b
  (L (\ a b (\ s (s* (a s) b))) a b))))
 vel (\ x xs . (foldl x xs (\ a b
  (L (\ a b (\ s (s+ (a s) (b s)))) a b)))))
(:
 ((sX x s) n m y) (y x s) ; cons onto a stream
 (sno n) (n) ; empty stream
 (sun x) (sX x sno) ; singleton stream

 (s+ s t) (s (\ t) ; append streams
             (\ s (\ _ m (m (s+ t s))))
             (\ x s (sX x (s+ s t))))

 (s* s g) (s (\ s) ; flatmap goal over stream
             (\ s (\ _ m (m (s* s g))))
             (\ x s (s+ (g x) (s* s g))))

 (stake i s) ; stream take
  (? (nilp i) sno
   (s (\ s)
      (\ s (stake i s))
      (\ x s (\ _ _ y (y x (stake (- i 1) s))))))

 (smap f s) ; stream map
  (s (\ s)
     (\ s (\ _ m (m (smap f s))))
     (\ x s (sX (f x) (smap f s))))

 (sfold f z s) ; stream fold
  (s (\ z)
     (\ s (sfold f z s))
     (\ x s (f x (sfold f z s))))

 (slist s) (sfold X 0 s) ; stream -> list

 ; unify
 ((est u v) s) (:
  (walk u s) (?
   (nilp (var? u)) u
   (: t (has-s s u)
    (? t (walk (A t) s) u)))
  x (walk u s) y (walk v s) (?
   (= x y)  (sun s)
   (var? x) (sun (ext-s s x y))
   (var? y) (sun (ext-s s y x))
   (&& (twop x) (twop y))
    ((et (est (A x) (A y)) (est (B x) (B y))) s)
   sno))

 (Run n ks g) (:
  (walk* u s) (?
   (var? u) (: t (has-s s u) (? t (walk* (A t) s) u))
   (twop u) (X (walk* (A u) s) (walk* (B u) s))
   u)
  (re st) (flat-map ks (\ k
   (: it (has-s (X (map (A st) A ) (B st)) k)
    (? it (L k (walk* (A it) st))))))
  (slist (smap re (? n (stake n (g ~~)) (g ~~))))))

(test k2
 (unord-eq '((a (1 2 (3 (4))) b (3 (4)) c 4)) (Run 0 '(a b c)
  (\\ a b c (et (est a (L 1 2 b))
                (est b (L 3 (L c)))
                (est c 4)))))
 (= (rho 9 '(a 9)) (Run 9 '(a)
  (\\ a (: nines (vel (est a 9) (zz nines))))))
 (unord-eq (map (iota 10) (\ i (L 'a i))) (Run 10 '(a)
  (\\ a (:
   (at-least n) (vel (est a n) (zz (at-least (+ n 1))))
   (vel (\ sno) (est a 0) (at-least 1))))))
 (unord-eq '((a 7 b 6) (a 7 b 5)) (Run 0 '(a b)
  (\\ a b (et (est a 7) (vel (est b 5) (est b 6))))))
 (unord-eq '((a 9 b 8)) (Run 0 '(a b)
  (\\ a b (et (est a 9) (est b 8)))))
 (unord-eq '((a 1 b 1)) (Run 0 '(a b)
  (\\ a b (et (est a 1) (est a b)))))
 (unord-eq '((a 7 b 3)) (Run 0 '(a b)
  (\\ a b (et (est a 7)
              (est b 3)
              (vel (est b 3) (est b 4))))))
 (unord-eq '((a (1 2) b 2)) (Run 0 '(a b)
  (\\ a b (et (est a (L 1 b)) (est b 2)))))
 (nilp (Run 0 '(a) (\\ a (et (est a 0) (est a 1)))))
 (nilp (Run 0 '(a b) (\\ a b
  (et (est a 7) (est b 3) (vel (est b 2) (est b 4))))))
 (unord-eq '((a 1 b 2 c 2) (b 9 c 7) (c 88))
  (Run 0 '(a b c) (\\ a b c (vel
   (et (est a 1) (est b 2) (est c b))
   (et (est c 7) (est b 9))
   (est c 88)))))
 (unord-eq '((a 7 b 8 c 9) (a 4 b 5 c 6) (a 1 b 2 c 3))
  (Run 0 '(a b c) (\\ a b c (vel
   (et (est a 1) (est b 2) (est c 3))
   (et (est a 4) (est b 5) (est c 6))
   (et (est a 7) (est b 8) (est c 9))))))
 (unord-eq (map (iota 10) (\ i (L 'a i)))
  (Run 10 '(a) (\\ a
   (: never (vel (\ sno) (zz never))
      (at-least n) (vel (est a n) (zz (at-least (+ n 1))))
    (vel never (est a 0) (at-least 1))))))
 (: inc L
    (iter n f x) (? (= 0 n) x (iter (- n 1) f (f x)))
    zero 0 one '(0) two '((0))
    (add a b c) (vel (et (est a 0) (est b c))
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
    (= (L (L 'a (iter 144 L 0)))
     (Run 0 '(a) (\\ a (fibo (iter 12 L 0) a))))))
