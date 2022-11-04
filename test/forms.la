(test "special forms"
 ; progn
 (= () (,) (, ()) (, 1 2 3 ()))
 ; lambda
 (= () ((\)))
 (= 99 ((\ 99)) ; thunk
       ((\ a a) 99) ; one arg
       ((\ a b (+ b a)) 54 45) ; two args
       ((\ xs . (ap * xs)) 3 3 11)) ; vararg
 ; let
 (= () (:))
 (= 3 (: a 1 b 2 (+ a b)) ; odd like letrec
      ((\ n (: a n)) 3)) ; even like define
 ; if
 (= () ; even forms
  (?) ; nullary
  (? () 1) ; fallthrough
  (? 1 ()))
 (= 1 ; odd forms
   (? 1)
   (? 1 1 2)
   (? 1 1 2 3)
   (? 0 0 0 2 1)
   (? 0 0 0 1 0 2 0 3 1)))
