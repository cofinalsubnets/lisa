(:
 (term-esc string) (, (putc 27) (puts string))
 green-dot '"[32m."
 red-dot '"[31m."
 reset '"[0m"
 (term-text s)
  (, (putc 27)
     (puts s)
     (putc 27)
     (puts reset)))

(: test-state (tnew 0)
   test-set (tset test-state)
   test-get (tget 0 test-state))
(:
 (ttt l) (:
 (report x v) (,
  (test-set 'count (+ 1 (test-get 'count)))
   (? v (term-text green-dot)
    (, (test-set 'fail (X x (test-get 'fail)))
       (term-text red-dot)
       (. x))))
 (f l) (? (X? l)
  (L ', (L report (L '` (A l)) (A l)) (f (B l))))
 (f l)))

(:: 'assert ttt)
(:: 'T ttt)
