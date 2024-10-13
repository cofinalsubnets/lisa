; heron's method for finding square roots
(: (heron x g) (? (= g (/ x g)) g
    (heron x (/ (+ g (/ x g)) 2)))
   guess 47032
 (assert (= 6 (heron 36 guess))))
