; shared kanren support code
(:
 ; dict - immutable map type made of pairs
 (dict-has d k) (:
  (loop k ks vs) (? ks
   (? (= k (A ks)) vs (loop k (B ks) (B vs))))
  (loop k (A d) (B d)))
 (dict-get d k) (? (: x (dict-has d k)) (A x))
 (dict-set d k v) (X (X k (A d)) (X v (B d)))

 (anon? x) (&& (symp x) (nilp (ystr x)))
 (var? x) (&& (2? x) (anon? (B x)))
 (var x) (X x (sym))

  has-s dict-has
  ref-s dict-get
  (ext-s s k v) (dict-set s k v)
  st-empty ~~
 )
(:::
 \\ (\ a . (X (X '\ a) (map (\ a (L var (Q a))) (init a))))
 zzz (\ x (: y (sym) (L \ y  (L \ (L x y))))))
