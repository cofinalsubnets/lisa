;(. 0 'helpers)
(:
 (term-esc string) (puts (ap str (X 27 (chars string))))
 (green) (term-esc "[32m")
 (red)   (term-esc "[31m")
 (reset) (term-esc "[0m")

 (color-text col s) (, (col) (puts s) (reset))
 (green-text s) (color-text green s)
 (red-text s) (color-text red s)

 *test* (tbl 'failures (tbl)))

(:
 (mut t k f) (tset t k (f (t k)))
 (:::
  test (:
   (\ n ts . (:
    (test x) (L '? x
     (L ', (L mut *test* ''count inc)
           (L green-text "."))
     (L ', (L mut (*test* 'failures) (Q n) (cu X x))
           (L red-text "X")))
    (, (X ', (map ts test))))))))

