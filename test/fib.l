(: (fib n) (? (> 3 n) 1 (+ (fib (+ -1 n)) (fib (+ -2 n))))
 (assert (= 2178309 (fib 32))))
