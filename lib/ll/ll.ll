(: macros (tbl)
   _ns (cwm))
; to bootstrap first we define an expression that when
; eval'd defines the standard environment and compiler, and
; redefines eval. then we eval it twice. the first time
; everything is compiled by the bootstrap system, the second
; time by the (bootstrapped) live system.
(: prelude '(,
  (: ; some aliases
     `'` ?'? :': ,', \'\ ~~ '(())
     ~ nilp 2p twop Hp homp Tg tget Ts tset Td tdel Th thas
     2? twop \? homp Y? symp

     (AA x) (A (A x)) (AB x) (A (B x))
     (BA x) (B (A x)) (BB x) (B (B x))

     ; functional programmings
     (I x) x id I
     (K x) (\ x) const K
     (co xs .) (foldl1 xs (\ m f (\ x . (f (ap m x)))))
     ((flip f) x y) (f y x)
     ((cu f x .) y .) (ap f (append x y))
     (map f x) (? (2? x) (X (f (A x)) (map f (B x))) x)
     (append a b) (? (2? a) (X (A a) (append (B a) b)) b)

     (flat-map x f) (? (2? x) (append (f (A x)) (flat-map (B x) f)))
     ; logic
     (&& l .) (all id l)
     (|| l .) (any id l)
     (each x f) (? x (, (f (A x)) (each (B x) f)))
     (all p l) (? l (? (p (A l)) (all p (B l))) 1)
     (any p l) (? l (? (p (A l)) 1 (any p (B l))))
     (part p l) (foldr l ~~ (\ a m
      (? (p a) (X (X a (A m)) (B m))
               (X (A m) (X a (B m))))))

     ; data constructors
     (L . .) .
     (Q x) (L ` x)

     ; tables
     (set x .) (foldl x (tbl) (\ t x (, (Ts t x ()) t)))

     ; numbers
     (inc x) (+ x 1)
     (dec x) (- x 1)

     ; lists
     (at l n) (? (< n 1) (A l) (at (B l) (- n 1)))
     (filter x p) (foldr x () (\ x m (? (p x) (X x m) m)))
     (foldr x z f) (? (2? x) (f (A x) (foldr (B x) z f)) z)
     (foldl x z f) (? (2? x) (foldl (B x) (f z (A x)) f) z)
     (foldl1  x f) (foldl (B x) (A x) f)
     (foldr1  x f) (?
      (2? (B x))
       (f (A x) (foldr1 (B x) f)) 
      (A x))
     (zip f a .) (? (? a (all id a))
      (X (ap f (map A a)) (ap zip (X f (map B a)))))
     (snoc l x) (? l (X (A l) (snoc (B l) x)) (L x))
     (randn n) (% (abs (rand)) n)
     (init l) (? (B l) (X (A l) (init (B l))))
     (last l) (? (B l) (last (B l)) (A l))
     (memq x k) (? x (? (= k (A x)) x (memq (B x) k)))
     (del l k) (filter l (\ x (~ (= x k))))
     (idx l x)
      (: (loop l x n)
          (? l (? (= x (A l)) n (loop (B l) x (+ n 1))))
       (loop l x 0))
     (len l) (foldl l 0 inc)

     )

  (tset macros
   '::: (: (defm n x .) (X ', (X (L tset macros (Q n) (A x))
                                 (? (B x) (ap defm (B x)))))
         defm)
   'imm ev
   'AA (\ x (L A (L A x))) 'AB (\ x (L A (L B x)))
   'BA (\ x (L B (L A x))) 'BB (\ x (L B (L B x)))
   'AAA (\ x (L A (L A (L A x)))) 'AAB (\ x (L A (L A (L B x))))
   'ABA (\ x (L A (L B (L A x)))) 'ABB (\ x (L A (L B (L B x))))
   'BAA (\ x (L B (L A (L A x)))) 'BAB (\ x (L B (L A (L B x))))
   'BBA (\ x (L B (L B (L A x)))) 'BBB (\ x (L B (L B (L B x))))
   'char (\ x (sget x 0))
   '&& (\ x . (? x ((: (& x) (? (B x) (L ? (A x) (& (B x))) (A x))) x) 1))
   '|| (\ x . ((: (| x) (? x (: q (sym) (L ': q (A x) (L ? q q (| (B x))))))) x))
   '>>= (\ x . (? (> (len x) 1) (X (last x) (init x))
                                (: y (sym) (L \ y (X y x)))))
   'case (\ x a . (:
     y (sym)
     (loop a)
      (? a (? (~ (B a)) (A a)
       (L ? (L '= y (A a)) (A (B a))
        (loop (B (B a))))))
    (L ': y x (loop a)))))
  )

  )
; this is the compiler source that's eval'd by stage 1, then
; again by itself to produce the final eval function. you can't
; use any macros in here, because stage 1 doesn't know about
; macros.
(: egg '(, (:
   ; XXX be systematic about this
   pure (set
    L X A B Q + - * / % & | ^ << >> I K flip inc dec co
    = ~ <= >= < > cu const snoc init last memq at idx len
    twop nilp symp nump tblp strp homp
    && || scat slen sget ssub str)

   ; these enable extra partial evaluation for functions
   ; with certain algebraic properties. the nullary case for
   ; any grouplike function has to return its structure's
   ; identity element.
   abelians (set && || + *) ; arguments can be combined freely
   monoids (set scat append) ; pairs of consecutive arguments can be combined in place


 ; instruction fusion table
 fuse (:

  ; a common case is to rewrite the last emitted
  ; instruction or value according to an exact table
  ; of values
  ((f1 g xs .) i p)
   ((\ a q
    ((: (loop x) (?
     (~ x)       (emi i p)
     (= a (A x)) (emi (AB x) q)
     (loop (BB x)))) xs))(g p) (hseek p 1))

  ; for specialized indexed instructions
  (argn a b) (f1 hgetx 0 a 1 b)

  ; for specialized branch instructions ;
  ; takes an argument for each branch case
  (br2 b c) (f1 hgeti i-branch b i-barnch c)

  (tbl
   ; this along with "return forwarding" in branches is how
   ; tail calls get optimized.
   i-call (\ i h (? (= i-ret (hgeti (hseek h 1)))
    (emi i-rec (emx (hgetx h) (hseek h 1)))
    (emi i h)))
   i-arg (argn i-arg0 i-arg1)
   i-loc (argn i-loc0 i-loc1)
   i-clo (argn i-clo0 i-clo1)
   i-lt (br2 i-brlt i-brlt2)
   i-lteq (br2 i-brlteq i-brlteq2)
   i-gt (br2 i-brgt i-brgt2)
   i-gteq (br2 i-brgteq i-brlt)
   i-eq (br2 i-breq i-brne)
   i-nilpp (f1 hgeti i-branch i-barnch i-barnch i-branch)
   i-imm (f1 hgetx 0 i-zero 1 i-one)))

 ; special function compiler table
 inliners (:

  (mono f m x) (:
   (fold f m x) (? (~ x) (L m)
    (? (qtd (: y (A x))) (fold f (f m (unq y)) (B x))
     (: z (fold f m (B x)) (X (A z) (X y (B z))))))
   (? (= (f) (A (: j (fold f m x)))) (B j) j))

  ((ordr i) x k) (?
   (~ x) (Im () k)
   (~ (B x)) (Im 1 k)
   (eval (A x) (:
    (fold k x y)
     (eval x (xc i (? (~ y) (push2 k)
      (xc i-barnch (peek2 (xc i-push
       (fold k (A y) (B y))))))))
    (xc i-push (pop2 (fold k (AB x) (BB x)))))))

  ((abel f i) x k) ((\ x
   (eval (A x) (>- 'num (2f i k (B x))))) (mono f (f) x))

  (nary i tR xs .) (: arity (len xs) (\ x k (?
   (< (len x) arity) (fail "wrong arity")
   (: (fo k z) (co (eval (AA z) (>- (BA z) k)) (B z))
      (co k z) (? z (fo (xc i-push k) z) k)
      l (xc i (-> tR k))
      z (zip X x xs)
    (? z (fo l z) l)))))

  (2f i k x)
   (: (2g i k x)
       (xc i-push (eval (A x) (>- 'num (xc i (2f i k (B x))))))
    (? x (2g i k x) (-> 'num k)))

  ((bitshift f i) x k) (:
   y (mono + 0 (B x))
   z (? (nump (A x)) (mono f (A x) y) (X (A x) y))
   (eval (A z) (>- 'num (2f i k (B z)))))

  (tbl
   id (\ x k (eval (A x) k))
   + (abel + i-add)
   - (\ x k (?
    (~ x) (Im 0 k)
    (~ (B x)) ((nary i-neg 'num 'num) x k)
    (: y (mono + 0 (B x))
       z (? (nump (A x)) (mono - (A x) y) (X (A x) y))
     (? z (eval (A z) (>- 'num (2f i-sub k (B z)))) (Im 0 k)))))
   * (abel * i-mul)
   / (\ x k (?
    (~ x) (Im 1 k)
    (~ (B x)) (eval (A x) (>- 'num k))
    (: b (mono * 1 (B x))
       c (? (nump (A x)) (mono / (A x) b) (X (A x) b)))
     (eval (A c) (>- 'num (2f i-dqv k (B c))))
    (Im 1 k)))

   & (abel & i-band)
   | (abel | i-bor)
   ^ (abel ^ i-bxor)
   >> (bitshift >> i-sar)
   << (bitshift << i-sal)

   A (nary i-car () 'two)
   B (nary i-cdr () 'two)
   X (nary i-cons 'two () ())
   L (\ x k (:
    (loop x k) (? (~ x) k
     (loop (B x)
      (xc i-push
       (eval (A x)
        (xc i-cons
         (-> 'two k))))))
    (Im () (loop x k))))


   emi (nary i-emi 'hom 'num 'hom)
   emx (nary i-emx 'hom () 'hom)

   tget (nary i-tget ()   'tbl ())
   tset (nary i-tset ()   'tbl () ())
   tlen (nary i-tlen 'num 'tbl)
   thas (nary i-thas ()   'tbl ())

   < (ordr i-lt) <= (ordr i-lteq)
   >= (ordr i-gteq) > (ordr i-gt)
   = (\ x k (?

    (|| (~ x)
     (? (: q (map unq (filter x qtd)))
        (: r (A q) (any (\ v (~ (= v r))) (B q)))))
     (Im () k)

    (~ (B x)) (Im 1 k)

    (eval (A x) (xc i-push (:
     (fold k x y) (:
      l (eval x (xc i-eq (? (~ y) k
         (xc i-barnch (peek2 (fold k (A y) (B y)))))))
      (? y (xc i-dupl l) l))
     (pop2 (fold (push2 k) (AB x) (BB x))))))))

   nilp (nary i-nilpp () ())
   twop (nary i-twopp () ())
   homp (nary i-hompp () ())
   nump (nary i-numpp () ())
   symp (nary i-sympp () ())
   strp (nary i-strpp () ())
   tblp (nary i-tblpp () ())
   fail (nary (hgeti fail) ())))

   ) (: ; new let expn makes sure ^^^ exist & don't make a closure vvv

 (type-sym-num x) (?
  (= x 'hom) 0
  (= x 'num) 1
  (= x 'two) 2
  (= x 'str) 3
  (= x 'sym) 4
  (= x 'tbl) 5
  (= x 0) 8)

 (type-sym x) (?
  (homp x) 'hom (nump x) 'num
  (twop x) 'two
  (symp x) 'sym (strp x) 'str
  (tblp x) 'tbl (fail))


 ; this function creates a new lexical environment.
 ; if the second argument (the parent scope) is nil,
 ; then the expression is compiled in the global scope.
 (env a p n)  (:
  (asig a n) (?
   (~ a) (X n a)
   (? (B a) (= '. (AB a))) (X (- (+ n 1)) (L (A a)))
   (: r (asig (B a) (+ n 1)) (X (A r) (X (A a) (B r)))))
  b (asig a 0)
  (tbl 'arg (B b) 'par p
       'nom n 'def (tbl)
    ;  'ty (L (L (tbl)))
       'val (tbl) 'sig (A b)))

 (toplp e) (~ (Tg e'par))


 ; this function is used in several places to look for a
 ; variable's value, or failing that to find where it's bound.
 (look e y) (?
  (Th (Tg e 'val) y)
   (X 'here (Tg (Tg e'val) y))
  ; toplevel bindings are special. if it's already defined
  ; we usually want to bind early, but we also want to allow
  ; redefinition, so in that case bind late.
  (toplp e) (: ns (cwm) (?
   (? (thas ns y) (~ (thas (tget e 'def) y)))
    (X 'here (tget ns y))
   (X 'wait ns)))
  (memq (Tg e 'clo) y) (X 'clo e)
  (memq (Tg e 'dfr) y) (X 'wait (Tg e'val))
  (memq (Tg e 'loc) y) (X 'loc e)
  (memq (Tg e 'arg) y) (X 'arg e)
  (look (Tg e 'par) y))

 ; ltu : compile a lambda expression.
 ; depending on if it encloses any variables, a lambda
 ; expression may or may not have a value at compile time. this
 ; function returns either a function or a pair. in the latter
 ; case A is the list of variables it encloses and B is the
 ; "prototype" thread that expects those variables to be
 ; available in the closure. the enclosing scope generates a
 ; constructor for the closure, which is composed with the
 ; prototype to create an "instance".
 (ltu e nom l) (:
  (loc k) (: loc (Tg f 'loc)
   (? (~ loc) k (emi i-locals (emx (len loc) k))))
  (ar k) (: i (Tg f 'sig) (?
   (> i 0) (emf i-arity (emx i k))
   (< i 0) (emi i-vararg (emx (- (- i) 1) k))
   k))

  n (? (B l) (B l) ~~)
  anom (? (: en (Tg e'nom)) (X nom en) nom)
  f (env (init n) e anom)
  x (weak f (last n))

  (?
   ; if the function is nonvariadic and all it does is apply
   ; a second known function to its exact list of arguments,
   ; then it's an alias for the second function.
   (? (>= (Tg f 'sig) 0)
      (? (2p x) (? (Hp (A x))
                   (= (B x) (Tg f 'arg)))))
    (A x)
   ; otherwise compile it
   (? (: k (hfin (ar (loc ((eval x ini) f 4))))
         clo (Tg f 'clo)) ; free variables or no ?
    (X clo k)
    k)))

 ; first compiler moiety : " weak evaluator "
 ; 1. construct the local scope of the function
 ; 2. desugaring, macroexpansion, and partial evaluation
 ;    (constant folding, dead code elimination, etc), which has
 ;    some preliminary "static type checking" as a side effect
 ; it populates the local variables list and the known values
 ; dictionary in the function environment, and returns an
 ; expression with the same value (under eval) as the input,
 ; but pre-evaluated "as much as possible". to tell if an
 ; expression was fully evaluated, check if the image is
 ; quoted.
 (qtd x) (? (2p x) (= (A x) '`) (~ (symp x)))
 (unq x) (? (2p x) (AB x) x)
 (quote x) (? (|| (symp x) (twop x)) (Q x) x)

 (weak e x)  (:
  ; function symbol sugar ; handles any level of nesting
  (sug x) (? (~ (2p (A x))) x
   (sug (X (AA x) (X (X '\ (snoc (BA x) (AB x)))(BB x)))))

  (loop y) (? y (,
   (? (qtd (: x (sug y) a (AB x)))
    (Ts (Tg e'val) (A x) (unq a))
    (, (Ts (Tg e'def) (A x) a) (rec a)))
   (loop (BB x))))

  (rec x) (? (2p x) (: head (A x) (?
   (|| (= head \) (= head `)) ()
   (= head :) (? (= 1 (% (len x) 2)) (loop (B x)))
   (each x rec))))

  (, (rec x) (resolve e) (wv e x)))

 ; this is a huge complicated procedure for "resolving
 ; the addresses" of sets of mutually recursive inner functions
 ; at compile time, so they can be as efficient as toplevel
 ; definitions whenever that's possible. it constructs the
 ; maximum set S of function variables whose definitions
 ; have no free variables not in S. then it arranges for
 ; references to functions in S to get resolved (with
 ; optimizations etc) later, which lets the compiler generate
 ; appropriate code without having a function address yet.
 (resolve e) (:
  ; step one: pass over each inner def for this function and
  ; try to rewrite and evaluate it. if this succeeds, add it
  ; to the known values. otherwise, if the definition is for
  ; a function, collect it into a list.
  (f1 kvs) (? kvs (?
   (Th (Tg e'val) (: kv (A kvs) k (A kv)))
    (f1 (B kvs))
   (: qq (wv e (B kv)) bb (f1 (B kvs)) (?
    (qtd (Ts (Tg e 'def) k qq))
     (, (Ts (Tg e 'val) k (unq qq)) (Td (Tg e 'def) k) bb)
    (? (2p qq) (= '\ (A qq)))
     (X (X k qq) bb)
    bb))))

  ; step two: now we have a list of function name/definition
  ; pairs. try and compile each one; if it succeeds, add it to
  ; the known values. otherwise collect the name, definition,
  ; and list of free variables into a list.
  (f2 kvs) (? kvs (: kv (A kvs) rr (f2 (B kvs)) (?
   (Hp (: i (ltu e (: k (A kv)) (: v (B kv)))))
    (, (Ts (Tg e'val) k i) (Td (Tg e'def) k) rr)
   (X (X k (X (A i) v)) rr))))

  ; step three: reject from the list all definitions that
  ; depend on values that won't be available until runtime.
  ; membership in the list is regarded as proof that the
  ; function is available now, so any time a function is
  ; rejected we start over again as it may have been
  ; referenced by a previously accepted function.
  (f3 kvs) (f3r kvs kvs ())
  (f3r i kvs q) (:
   (chk i vs) (? (~ vs) 1 (: v (A vs) (?
    (|| (any (\ j (= (A j) v)) i) (Th (Tg e'val) v))
     (chk i (B vs)))))
   (? (~ kvs) q (?
    (chk i (AB (: kv (A kvs))))
     (f3r i (B kvs) (X kv q))
    (f3 (del i kv)))))

  ; step four ( the last one ) : now we have the desired
  ; set of functions. remove them from the scope's local
  ; variables and add them to a list of deferred values, then
  ; recompile them. since we know each of these functions has
  ; no free variables that aren't being deferred, this time
  ; ltu will definitely return a function, which is stored in
  ; the known value set.
  (f4 kvs) (?
   (~ kvs) (Ts e'loc (tkeys (Tg e'def)))
   (, (Td (Tg e'def) (: kv (A kvs) k (A kv)))
      (Ts e 'dfr (X k (Tg e'dfr)))
      (f4 (B kvs))
      (Ts (Tg e'val) k (ltu e k (BB kv)))))

  ; only bother with any of this if there are inner defs
  (? (: ks (tkeys (: def (Tg e'def))))
   (f4 (f3 (f2 (f1 (map (\ k (X k (Tg def k))) (Ts e'loc ks))))))))

 (wv e x)  (:
  (list e x) (:

   ; static arity check : omit the check in the called function
   (check-arity x) (: z (A x) (?
    (~ (? (Hp z) (= i-arity (hgeti z)))) x
    (? (>= (len (B x)) (hgetx (hseek z 1)))
     (X (hseek z 2) (B x)) ; we're good, skip the check
     (fail)))) ; oops wrong arity, good thing we caught it at compile time !

   (defn e x) (:
    (nom s) (? (2p s) (nom (A s)) s)
    (sug d) (L (L \ (L , (init d) (last d))))
    (loop e x) (? x (:
     y (nom (A x))
     ll (loop e (BB x))
     (X y (X (Tg (Tg e 'def) y) ll))))
    (? (B x) (? (= 0 (% (len x) 2)) (sug x)
                (X : (loop e (B x))))))

   (lamb e l) (:
    (trlamp l) (? (B l) (: x (last l)
     (? (2p x) (? (= (A x) :) (= 0 (% (len x) 2))))))
    (wv-lamm l) (: m (last l)
     (snoc (init l) (L , (init m) (last m))))
    (? (trlamp l) (wv-lamm l) l))

   (mono f i l) (:
    (fold j x y) (?
     y (? (qtd x) (fold (f j (unq x)) (A y) (B y))
        (: z (X x (fold i (A y) (B y)))
         (? (= i j) z (X j z))))
     (qtd x) ((\ x (? (= i x) () (L x)))
              (f j (unq x)))
     (= i j) (L x)
     (L j x))
    (? l (X f (fold i (A l) (B l))) (quote i)))

   (cond e x) (:
    (rec e x) (?
     (~ x) (L ())
     (~ (B x)) (L (wv e (A x)))
     (~ (qtd (: a (wv e (A x)))))
      (X a (X (wv e (A (B x))) (rec e (BB x))))
     (unq a) (L (wv e (AB x)))
     (rec e (BB x)))
    (? (: r (rec e x)) (? (B r) (X ? r) (A r))))

   (seq e x) (:
    (loop e x) (? (~ (B x)) (L (wv e (A x))) (:
     z (loop e (B x))
     y (wv e (A x))
     (? (|| (symp y) (qtd y)) z (X y z))))
    (? (: y (B x)) (: z (loop e y)
                    (? (B z) (X ', z) (A z)))))


   a (A x) (?
    (= a `) (? (B x) x)
    (= a \) (lamb e x)
    (= a :) (defn e x)
    (= a ?) (cond e (B x))
    (= a ,) (seq e x)
    (? (Th macros a) (wv e (ap (Tg macros a) (B x)))
     (: z (map (\ x (wv e x)) x) q (A z) r (B z) (?
      (~ (Th pure q)) (check-arity z)
      (all qtd r) (quote (ap q (map unq r)))
      (Th monoids q) (mono q (q) r)
      (Th abelians q) (:
       ff (part qtd r)
       fg (ap q (map unq (A ff)))
       (X q (? (= fg (q)) (B ff) (X fg (B ff)))))
      (check-arity z))))))

  (symb e x) (: l (look e x) (?
   (= (A l) 'here) (quote (B l))
   (~ (thas (: defs (Tg e'def)) x)) x
   (: dd (wv e (Td defs x))
    (? (~ (qtd (Ts defs x dd))) x (,
     (Td (Tg e'def) x)
     (Ts (Tg e'val) x (unq dd))
     dd)))))

 (? (2p x) (list e x) (symp x) (symb e x) x))

 ; second moiety : " analyzing evaluator "
 ; generates code to produce the value of the expression
 ; produced by the first moiety.

 ((xc i k) e m) (emf i (k e (+ m 1)))
 ((xd x k) e m) (emx x (k e (+ m 1)))

 (emf i h) ((? (: q (Tg fuse i)) q emi) i h)
 (Im x k) (xc i-imm (xd x (-> (type-sym x) k)))

 ; these make functions that do certain side effects
 ; with env stack structures during code generation:
 ; either manipulate the stack or embed the item at the
 ; top into the thread.
 (((pushr s) k) e m) (: q (k e m) (A (Ts e s (X q (Tg e s)))))
 (((popr  s) k) e m) (: q (k e m) (, (Ts e s (B (Tg e s))) q))
 (((peekr s) k) e m) (: q (k e (+ m 1)) (emx (A (Tg e s)) q))

 push1 (pushr 's1) push2 (pushr 's2)
 pop1  (popr  's1) pop2  (popr  's2)
 peek1 (peekr 's1) peek2 (peekr 's2)

 (ev-t x t k) (eval x (>- t k))
 ; these functions are for type checking
 (>- t k) (? (~ t) k
  (xc (? (= t 'two) i-id2 (= t 'num) i-idZ
         (= t 'hom) i-idH (= t 'tbl) i-idT
         (fail)) k))

 (-> t k) (:
  (produces t h)
   (? (~ (: q (consumes h))) h
      (= t q) (produces t (hseek h 1))
      (fail))
  (co k (? t (cu produces t) id)))

 (consumes h) (: i (hgeti h) (?
  (= i i-idH) 'hom (= i i-idZ) 'num
  (= i i-id2) 'two (= i i-idT) 'tbl))

 (eval x k) (:
  (list x k) (:
   (cond x k) (:
    (loop x k) (:
     (fin e m) (:
      thd (k e (+ m 2))
      ex (A (Tg e's2))
      i (hgeti ex)
      (? (= i i-ret) (emi i thd) ; forward return instructions
       (emf i-jump ; otherwise jump somewhere ...
        (emx (? (= i i-jump) (hgetx (hseek ex 1)) ex) thd))))
     (? (~ (B x)) (eval (A x) fin)
      (eval (A x) (pop1 (xc i-branch (peek1
       (loop (BB x) (push1 (eval (AB x) fin)))))))))
    (pop2 (loop x (push2 k))))

   ((lamb x k) e m) (:
    (nom e k) ; try to pick a name for this function
     (: i (hgeti k) (?
      (= i i-tbind) (A (hgetx (hseek k 1))) ; maybe we're about to set a global?
      (= i i-loc_) (at (Tg e 'loc) (hgetx (hseek k 1))))) ; or a local

    (clo-thd e x k) (:
     (loop x k) (? (~ x) k
      (loop (B x) (eval (A x) (xc i-push k))))
     (k e m) (emi i-take (emx (len x) (hom (+ m 2))))
     ((loop x k) e 0))

    l ((-> 'hom k) e (+ m 2))
    y (ltu e (nom e l) x)
    (? (\? y) (emf i-imm (emx y l))
     (emi (? (Tg e'loc) i-encll i-encln)
      (emx (X (B y) (clo-thd e (A y) k)) l))))

   (seq x k) (? x (eval (A x) (seq (B x) k)) k)
   (app a b k) (:
    (loop x k) (? (~ x) k
     (loop (B x) (eval (A x) (xc i-push k))))
    (? (? (qtd a) (: q (Tg inliners (unq a)))) (q b k)
     (loop b (eval a (>- 'hom ; tail call optimization is handled by instruction fusion
      (xc i-call (xd (len b) k))))))) ; here is where you would do it instead

   (defn x k) (:
   ; the rewrite ensures the : expression is always an even form.
    (loop e x k) (? (~ x) k (:
     l (loop e (BB x) k)
     m (? (~ (|| (toplp e) (memq (Tg e'loc) (A x)))) l
          (toplp e) (xc i-tbind (xd (X (cwm) (A x)) l))
          (xc i-loc_ (xd (idx (Tg e'loc) (A x)) l)))
     (? (= 'here (A (: v (look e (A x)))))
      (Im (B v) m) (eval (AB x) m))))

    (\ e m ((loop e x k) e m)))

   (: z (A x) (?
    (= z `) (Im (AB x) k)
    (= z ?) (cond (B x) k)
    (= z :) (defn (B x) k)
    (= z \) (lamb x k)
    (= z ,) (seq (B x) k)
    (app z (B x) k))))

  ((symb x k) e m) (:
   (late d) (:
    h (k e (+ m 2))
    t (consumes h)
    c (type-sym-num t) ; get the runtime type tag
    (emf i-rslv (emx (X c (X d x)) (? t (hseek h 1) h))))

   s (A (: r (look e x))) (?
    (= s 'here) ((Im (B r) k) e m)
    (= s 'wait) (late (B r))
    (= e (B r)) (
     (xc (? (= s 'arg) i-arg (= s 'loc) i-loc (= s 'clo) i-clo)
      (xd (idx (Tg e s) x) k)) e m)
    (, (: l (len (Tg e'clo)))
       (Ts e'clo(snoc (Tg e'clo) x))
       ((xc i-clo (xd l k)) e m))))

  ((? (twop x) list (symp x) symb Im) x k))

 (ini e m) ((xc i-ret (xd (Tg e 'nom) (\ _ m (hom m)))) e m)
 (ev x) (: e (env () () ()) (((eval (weak e x) ini) e 0) x))
  ;; end of compiler namespace ; now return the eval function
;  ev ; comment this line to define internals at toplevel
  )))
; bootstrap

(ev prelude)
(tset _ns 'ev (ev egg))
(ev prelude)
(tset _ns 'ev (ev egg))
(tset macros 'imm ev)

; " drop privileges "
(: dels (cu (flip each) (cu Td _ns))
   insns (filter (tkeys _ns) (\ k (= "i-" (ssub (ystr k) 0 2))))
   locs '(cwm boot egg emi macros emx hom hseek hgeti hgetx hfin _ns)
 (, (Td macros 'i-)
    (each (L insns locs) dels)))
;ev
(:
     ((diag f) x) (f x x)

     ; strings
     (strip-prefix p s)
      (? (= p (ssub s 0 (: a (slen p)))) (ssub s a (slen s)))
     (chars s)
      (: (loop s n)
          (? (< n (slen s))
           (X (sget s n) (loop s (+ n 1))))
       (loop s 0))
     (puts s)
      (: (loop s n)
       (? (< n (slen s))
        (, (putc (sget s n))
           (loop s (+ n 1))))
       (loop s 0))
      (endl) (putc 10)
     (iota n x .)
      (: (loop m n) (? (< m n) (X m (loop (+ m 1) n)))
       (? (~ x) (loop 0 n) (loop n (A x))))
     (rho n xs .)
      (: (loop n x) (? (= n 1) x (append x (loop (- n 1) x)))
       (? (> n 0) (loop n xs)))
)
