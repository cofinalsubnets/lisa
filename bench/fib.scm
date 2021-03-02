(define (fib n)
  (if (< n 3) 1
    (+ (fib (- n 1)) (fib (- n 2)))))
(display (fib 32))
(newline)
