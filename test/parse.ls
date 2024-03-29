; explicit control parser combinators
(:
 (px-strip-prefix p s)
  (: a (slen p) (? (= p (ssub s 0 a)) (ssub s a (slen s))))
 ((px-lit l) s n y)
  (: suff (px-strip-prefix l s) (? suff (y suff l) (n s)))
 ((px-drop a) s n y) (a s n (\ s  (y s )))
 ((px-alt a b) s n y) (a s (\ (b s n y)) y)
 ((px-opt a) s _ y) (a s (\ (y s)) y)
 ((px-rep a) s n y) ((px-opt (px-cat a (px-rep a))) s n y)
 ((px-cat a b) s n y) (a s n (\ s p . (b s n (\ s q . (ap y (X s (append p q)))))))
 ((px-map p f) s n y) (p s n (\ s x . (ap y (X s (ap f x)))))
 (px-dot s n y) (? (> (slen s) 0) (y (ssub s 1 -1) (ssub s 0 1)) (n s))
 (px-one t) (foldl1 (map (chars t) (\ c (px-lit (str c)))) px-alt)
 ((px-sep-by a sep) s n y) ((px-alt (px-cat a (px-cat sep (px-sep-by a sep))) a) s n y)

 ws (px-drop (px-rep (px-alt (px-lit " ") (px-lit "
"))))
 digit (px-one "0123456789")
 digits (px-map (px-cat digit (px-rep digit)) (co scat L))
)

(: (digit? c) (<= (char "0") c (char "9"))
   (xdigit? c) (|| (digit? c)
                   (<= (char "a") c (char "f"))
                   (<= (char "A") c (char "F")))
   (bdigit? c) (<= (char "0") c (char "1"))
   (odigit? c) (<= (char "0") c (char "7"))
   (zdigit? c) (<= (digit? c)
                   (<= (char "a") c (char "b"))
                   (<= (char "A") c (char "B")))
   (lowercase? c) (<= (char "a") c (char "z"))
   (uppercase? c) (<= (char "A") c (char "Z"))
   (alpha? c) (|| (lowercase? c) (uppercase? c))
   (space? c) (|| (= c 32) (<= 9 c 13))
   )
(test "parsing expressions"
 (: input " 12    3
                4
                "
    get-digits (px-cat ws (px-cat (px-sep-by digits ws) ws))
  (= (get-digits input (\) L) '("" "12" "3" "4"))))
