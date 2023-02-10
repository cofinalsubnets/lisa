(.)
(:
 (show-name n) (?
  (symp n) (show-name (ynom n))
  (strp n) (, (red-text (scat "  " n)) (.)))
 (show-details x) (, (puts "    ") (. x))

 failures (tkeys (*test* 'failures))
 (,
   (? failures (,
    (. 'failures 'of)
    (each failures (\ k
     (, (show-name k)
        (each ((*test* 'failures)k) show-details))))))
   (. (*test* 'count) 'tests)
   (? failures
    (nope (tlen (*test* 'failures)) 'failures))))
