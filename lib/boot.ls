(: egg '(,

  (ev '(: ; prelude fns
  true -1 ~~ '(()) ~ nilp
  (L . .) .
  (Q x) (L '` x)

  (AA x) (A (A x)) (AB x) (A (B x))
  (BA x) (B (A x)) (BB x) (B (B x))

  ; functional programmings
  (id x) x
  ;(const x) (\ x) ; the "clean" definition uses a closure; more efficient to just make a thread!
  (const x) (poke i-imm x i-ret 'const (hom 4))
  (co xs .) (foldl1 xs (\ m f (\ x . (f (ap m x)))))
  ((flip f) x y) (f y x)
  ((cu f x .) y .) (ap f (append x y))
  (map x f) (? (twop x) (X (f (A x)) (map (B x) f)) x)
  (append a b) (? (twop a) (X (A a) (append (B a) b)) b)
  (cat xs .) (foldr xs 0 append)
  null-fn ( \ )

  ; logic
  (all l p) (? l (? (p (A l)) (all (B l) p)) true)
  (any l p) (? l (? (p (A l)) true (any (B l) p)))
  (&& l .) (all l id)
  (|| l .) (any l id)

  (each x f) (? x (, (f (A x)) (each (B x) f)))
  (part p l) (foldr l ~~ (\ a m
   (? (p a) (X (X a (A m)) (B m))
            (X (A m) a (B m)))))

  ; tables
  (set x .) (foldl (tbl) x (\ t x (, (tset t x true) t)))

  ; numbers
  (inc x) (+ x 1) (dec x) (- x 1)

  ; lists
  (at l n) (? (< n 1) (A l) (at (B l) (- n 1)))
  (filter x p) (foldr x () (\ x m (? (p x) (X x m) m)))
  (foldr x z f) (? (twop x) (f (A x) (foldr (B x) z f)) z)
  (foldl z x f) (? (twop x) (foldl (f z (A x)) (B x) f) z)
  (foldl1  x f) (foldl (A x) (B x) f)
  (foldr1  x f) (?
   (twop (B x))
    (f (A x) (foldr1 (B x) f))
   (A x))
  (zip f a .) (? (all a id)
   (X (ap f (map a A)) (ap zip (X f (map a B)))))
  (len l) (foldl 0 l inc)
  (snoc l x) (? l (X (A l) (snoc (B l) x)) (L x))
  (init l) (? (B l) (X (A l) (init (B l))))
  (last l) (? (B l) (last (B l)) (A l))
  (memq x k) (? x (? (= k (A x)) x (memq (B x) k)))
  (del l k) (filter l (\ x (nilp (= x k))))
  (idx l x)
   (: (loop l x n)
       (? l (? (= x (A l)) n (loop (B l) x (+ n 1))))
    (loop l x 0))))

 (ev '(tset macros
  '::: (:
   (defm n x .) (X ',
    (X (L tset macros (L '` n) (A x))
     (? (B x) (ap defm (B x)))))
   defm)
  'literal ev
  'AA (\ x (L A (L A x))) 'AB (\ x (L A (L B x)))
  'BA (\ x (L B (L A x))) 'BB (\ x (L B (L B x)))
  'AAA (\ x (L A (L A (L A x)))) 'AAB (\ x (L A (L A (L B x))))
  'ABA (\ x (L A (L B (L A x)))) 'ABB (\ x (L A (L B (L B x))))
  'BAA (\ x (L B (L A (L A x)))) 'BAB (\ x (L B (L A (L B x))))
  'BBA (\ x (L B (L B (L A x)))) 'BBB (\ x (L B (L B (L B x))))
  'char (\ x (schr x 0))
  '&& (\ x . (? x ((: (& x) (? (B x) (L '? (A x) (& (B x))) (A x))) x) true))
  '|| (\ x . (: q (sym) (foldr x 0 (\ x m (L (L '\ q (L '? q q m)) x)))))
  '>>= (\ x . (X (last x) (init x)))
  'where: (\ x dfns . (X ': (snoc dfns x)))))

(ev '(: ; compiler data
  ; XXX be systematic about this!
  ; obviously we should be able to tell a function is pure
  ; if it's composed of pure functions/operations ...
  pure (set
   L X A B Q + - * / % & | ^ << >> id const flip co
   = ~ <= >= < > cu snoc init last memq at idx len
   twop nilp symp nump tblp strp homp append cat
   && || scat slen schr ssub str)

  ; these enable extra partial evaluation for functions
  ; with certain algebraic properties. the nullary case for
  ; any grouplike function has to return its structure's
  ; identity element.
  abelians (set && || + * | &) ; arguments can be combined freely
  monoids (set scat cat) ; pairs of consecutive arguments can be combined in place

  fuse (: ; instruction fusion table
   ; a common case is to rewrite the last emitted
   ; instruction or value according to an exact table
   ; of values
   ((f1 xs .) i p) (:
    (loop i p a x) (?
     (nilp x) (poke i p)
     (= a (A x)) (pokef (AB x) (seek p 1))
     (loop i p a (BB x)))
    (loop i p (peek p) xs))

   ; for idempotent instructions
   (idem i h) (? (= i (peek h)) h (poke i h))

   ; for specialized indexed instructions
   (argn xs .) (:
    (loop n xs) (? xs (X n (X (A xs) (loop (+ n 1) (B xs)))))
    (ap f1 (loop 0 xs)))

   ; for specialized branch instructions ;
   ; takes an argument for each branch case
   (br2 b c) (f1 i-br1 b i-br0 c)

   ((pushi i) j p) (? (= i-push (peek p)) (pokef i (seek p 1))
                                          (poke j p))
   (tbl
    i-argn (argn i-arg0 i-arg1 i-arg2 i-arg3)
    i-sl1n (argn i-sl10 i-sl11 i-sl12 i-sl13)
    i-clon (argn i-clo0 i-clo1 i-clo2 i-clo3)
    i-lt (br2 i-brl i-brge)
    i-lteq (br2 i-brle i-brg)
    i-gt (br2 i-brg i-brle)
    i-gteq (br2 i-brge i-brl)
    i-eq (br2 i-bre i-brn)
    i-nilp_ (f1 i-br1 i-br0 i-br0 i-br1)
    i-imm (f1 0 i-imm0 1 i-imm1 -1 i-immn1)
    i-arity (f1 1 i-ary1 2 i-ary2 3 i-ary3 4 i-ary4)
    i-imm1 (pushi i-imm1p)
    i-immn1 (pushi i-immn1p)
    i-imm0 (pushi i-imm0p)
    i-arg0 (pushi i-arg0p)
    i-arg1 (pushi i-arg1p)
    i-arg2 (pushi i-arg2p)
    i-arg3 (pushi i-arg3p)
    i-clo0 (pushi i-clo0p)
    i-clo1 (pushi i-clo1p)
    i-clo2 (pushi i-clo2p)
    i-clo3 (pushi i-clo3p)
    i-sl10 (pushi i-sl10p)
    i-sl11 (pushi i-sl11p)
    i-sl12 (pushi i-sl12p)
    i-sl13 (pushi i-sl13p)
    i-idmo idem
    i-idtwo idem
    i-idno idem
    i-idtbl idem))

 ; special function compiler table
 ; each entry has type ((value) m e) x k
  inliners (:

   (in-mono f m x) (:
    (fold f m x) (? (nilp x) (L m)
     (? (quoted? (A x)) (fold f (f m (unquote (A x))) (B x))
      (: z (fold f m (B x)) (X (A z) (X (A x) (B z))))))
    j (fold f m x)
     (? (= (f) (A j)) (B j) j))

   ; for comparison operators
   ((compar f i) x k) (?
    (|| (nilp x) (nilp (B x))) (imm true k)
    (nilp (BB x))
     (co-ev (A x) (emc i-push (co-ev (AB x) (emc i k))))
    (co-apply f x k))

   ((in-abel f i) x k) (: y (in-mono f (f) x)
    (binop 'num i k y))

   ; for operations with a defined arity
   (typed i ret-type arg-types .) (\ x k (?
    (< (len x) (len arg-types)) (nope 'wrong 'arity ': (len x) 'of (len arg-types))
    (: (fo k z) (co (co-ev (AA z) (consumes (BA z) k)) (B z))
       (co k z) (? z (fo (emc i-push k) z) k)
       l (emc i (produces ret-type k))
       z (zip X x arg-types)
     (? z (fo l z) l))))

   (binop t i k x)
    (co-ev (A x) (foldr (B x) k (\ x k
     (consumes t (emc i-push (co-ev x (consumes t (emc i (produces t k)))))))))

   ((bitshift f i) x k) (:
    y (in-mono + 0 (B x))
    z (? (nump (A x)) (in-mono f (A x) y) (X (A x) y))
    (binop 'num i k z))

   (conses x k) (foldl k x (\ k x
    (emc i-push (co-ev x (emc i-cons (produces 'two k))))))

   (tbl
    id (\ x k (co-ev (A x) k))
    + (in-abel + i-add)
    - (\ x k (?
     (nilp x) (imm 0 k)
     (nilp (B x)) ((typed i-neg 'num 'num) x k)
     (: y (in-mono + 0 (B x))
        z (? (nump (A x)) (in-mono - (A x) y) (X (A x) y))
      (? z (binop 'num i-sub k z) (imm 0 k)))))
    * (in-abel * i-mul)
    / (\ x k (?
     (nilp x) (imm 1 k)
     (nilp (B x)) (co-ev (A x) (consumes 'num k))
     (: b (in-mono * 1 (B x))
        c (? (nump (A x)) (in-mono / (A x) b) (X (A x) b))
      (? c (binop 'num i-quot k c) (imm 1 k)))))

    & (in-abel & i-band)
    | (in-abel | i-bor)
    ^ (in-abel ^ i-bxor)
    >> (bitshift >> i-sar)
    << (bitshift << i-sal)

    A (typed i-car 0 'two)
    B (typed i-cdr 0 'two)
    X (\ x k (? (nilp x) (imm 0 k)
     (co-ev (last x) (conses (init x) k))))

    L (\ x k (imm 0 (conses x k)))


    tget (typed i-tget 0    'tbl 0)
    tset (typed i-tset 0    'tbl 0 0)
    tlen (typed i-tlen 'num 'tbl)
    thas (typed i-thas 'num 'tbl 0)

    < (compar < i-lt) <= (compar <= i-lteq)
    >= (compar >= i-gteq) > (compar > i-gt)
    = (compar = i-eq)

    ; FIXME these should be variadic
    nilp (typed i-nilp_ 'num 0)
    twop (typed i-twop_ 'num 0)
    homp (typed i-homp_ 'num 0)
    nump (typed i-nump_ 'num 0)
    symp (typed i-symp_ 'num 0)
    strp (typed i-strp_ 'num 0)
    tblp (typed i-tblp_ 'num 0))))) ; end inliners

 (ev '(: ; compiler source
  (pokef i h) (>>= i h (fuse i) (\ i h q ((? q q poke) i h)))
  ((emc i k) m e) (pokef i (k (+ m 1) e))
  ((emd x k) m e) (poke x (k (+ m 1) e))

  (quoted? x) (? (twop x) (= (A x) '`) (nilp (symp x)))
  (unquote x) (? (twop x) (AB x) x)
  (quote x) (? (|| (symp x) (twop x)) (L '` x) x)
  (lambda? x) (&& (twop x) (= '\ (A x)))
  (letrec? x) (&& (twop x) (= ': (A x)) (& 1 (% (len (B x)) 2)))
  (rw-letrec l) (: m (last l)
   (snoc (init l) (L ', (init m) (last m))))
  (pure-x? x) (|| (symp x) (quoted? x) (lambda? x) (&&
   (twop x) (pure (A x)) (all (B x) pure-x?)))

  (toplp e) (nilp (e 'par))

  ; this function is used in several places to look for a
  ; variable's value, or failing that to find where it's bound.
  (look e y) (?
   (thas (e 'val) y) (X 'here ((e 'val) y))
   (toplp e)
    ; toplevel bindings are special. if it's already defined
    ; we usually want to bind early, but we also want to allow
    ; redefinition, so in that case bind late.
    (? (&& (thas _ns y) (nilp (thas (e 'def) y)))
     (X 'here (_ns y))
     (X 'wait _ns))
   (memq (e 'loc) y) (X 'loc e)
   (memq (e 'arg) y) (X 'arg e)
   (memq (e 'clo) y) (X 'clo e)
   (memq (e 'dfr) y) (X 'wait (e 'val))
   (look (e 'par) y))

   (wev e x) (:
    (rw-sym e x) (: l (look e x) (?
     (= 'here (A l)) (quote (B l))
     ; FIXME what is this even doing? it should probably be in resolve
     (&& (thas (e 'def) x)
         (quoted? (tset (e 'def) x (wev e (tdel (e 'def) x)))))
      (tset (e 'val) x (unquote (tdel (e 'def) x)))
     x))

    (rw-list e x) (:
     ; FIXME optimization
     ; static arity check : omit the check in the called function.
     ; assumes the arity check, if present, is the 1st instruction.
     (check-arity x) (: z (A x) (?
      (nilp (&& (homp z) (= i-arity (peek z)))) x
      (? (>= (len (B x)) (peek (seek z 1)))
       (X (seek z 2) (B x)) ; this call is ok, skip the check
       (, (ap z (B x)) ; oops wrong arity, call the function to get an error
          0)))) ; avoid a tail call to get a more informative backtrace

     (rw-let e x) (:
      (nom s) (? (twop s) (nom (A s)) s)
      (loop e x) (? x (:
       y (nom (A x))
       ll (loop e (BB x))
       (X (L ': y ((e 'def) y)) ll)))
      (? (& 1 (len (B x))) x
         (X ', (loop e (B x)))))

     (rw-mono f l) (:
      (fo acc x) (? (nilp x) (L (quote acc))
       (? (quoted? (A x)) (fo (f acc (unquote (A x))) (B x))
          (X (quote acc) (X (A x) (fo (f) (B x))))))
      (X f (del (fo (f) l) (f))))

     (rw-cond e x) (:
      (rec x) (?
       (nilp x) 0
       (nilp (B x)) (wev e (A x))
       (: a (wev e (A x)) (?
        (nilp (quoted? a))
         (L '? a (wev e (A (B x))) (rec (BB x)))
        (unquote a) (wev e (AB x))
        (rec (BB x)))))
      (rec (B x)))

     (rw-begin e x) (? (B x) (:
      (loop e x) (? (nilp (B x)) (L (wev e (A x)))
       (>>= (loop e (B x)) (wev e (A x)) (\ z y
        (? (pure-x? y) z (X y z)))))
      (X ', (loop e (B x)))))

     a (A x) (?
      ; special forms
      (= '` a) x
      (= '\ a) x
      (= ': a) (rw-let e x)
      (= '? a) (rw-cond e x)
      (= ', a) (rw-begin e x)
      ; is it a macro ?
      (macros a) (wev e (ap (macros a) (B x)))
      ; nope, it's a function call.
      (: z (map x (\ x (wev e x))) q (A z) r (B z) (?
        ; if it's pure & its arguments are all here then we can call it now.
        (&& (pure q) (all r quoted?))
         (quote (ap q (map r unquote)))
       ; if it's an abelian operator we may be able to partially apply it
        (abelians q) (:
         ff (part quoted? r)
         fg (ap q (map (A ff) unquote))
         (X q (? (= fg (q)) (B ff) (X fg (B ff)))))
        ; or if it's monoidal ...
        (monoids q) (rw-mono q r)
        ; otherwise we can at least try and check the arity.
        (check-arity z)))))

    (? (twop x) (rw-list e x) (symp x) (rw-sym e x) x))

  ; ltu : compile a lambda expression.
  ; depending on if it encloses any variables, a lambda
  ; expression may or may not have a value at compile time. this
  ; function returns either a function or a pair. in the latter
  ; case A is the list of variables it encloses and B is the
  ; "prototype" thread that expects those variables to be
  ; available in the closure. the enclosing scope generates a
  ; constructor for the closure, which is composed with the
  ; prototype to create an "instance".
  (ltu- lx nom b0) (? b0
   (ltu lx nom (init b0) (last b0))
   (ltu lx nom 0 0))
  (ltu par nom args xpn) (:
   ; this function creates a new lexical environment.
   ; if the second argument (the parent scope) is nil,
   ; then the expression is compiled in the global scope.
   (env arg par nom) (:
    (ary-sig a n) (?
     (nilp a) (X n a)
     (? (B a) (= '. (AB a))) (X (- (+ n 1)) (A a) 0)
     (: r (ary-sig (B a) (+ n 1))
      (X (A r) (A a) (B r))))
    sig-arg (ary-sig arg 0)
    (tbl 'arg (B sig-arg) 'par par 'nom nom ;'lam (tbl)
         'def (tbl) 'val (tbl) 'sig (A sig-arg)))

   (co-ini m e) (poke i-ret (? (toplp e) 'ev (e 'nom)) (hom (+ m 2)))
   ((w/locals k) m e) (>>= (k (+ m 2) e) (\ k (?
    (e 'loc) (pokef i-setloc (poke (len (e 'loc)) k))
    k)))
   ((w/arity k) m e) (>>= (k (+ m 2) e) (e 'sig) (\ k sig (?
    (> sig 0) (pokef i-arity (poke sig k))
    (< sig 0) (pokef i-varg (poke (- (- sig) 1) k))
    k)))

   full-name (?
    (nilp par) nom
    (nilp nom) (par 'nom)
    (par 'nom) (X nom (par 'nom))
    nom)
   chi (env args par full-name)
   out (hfin ((w/arity (w/locals (co-ev (weak-ev chi xpn) co-ini))) 0 chi))
   (? (chi 'clo) ; are there closure variables?
       (X (chi 'clo) out) ; if so return a pair of the vars and the thread.
      out)) ; otherwise return the fully compiled function.

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

  (desugar-dfns dfns) (?
   (|| (nilp dfns) (nilp (B dfns))) dfns
   (: (desug k v ds)
       (? (nilp (twop k)) (X k v ds)
        (desug (A k) (X '\ (snoc (B k) v)) ds))
    (desug (A dfns) (AB dfns) (desugar-dfns (BB dfns)))))

  ; partial evaluator
  (weak-ev e x)  (:

   (scan-let dfns) (? dfns (,
    (scan (tset (e 'def) (A dfns) (AB dfns)))
    (scan-let (BB dfns))))

   ; scan an expression for variable definitions.
   (scan x) (? (twop x) ; skip atoms
    (: head (A x) (?
     (= '` head) 0 ; skip quotations
     (= '\ head) 0 ;(co-lam- e 0 x) ; skip lambdas FIXME really?
     (= ': head) ; descend into *even* let expressions
      ; (odd let expressions will be rewritten into lambdas)
      (? (& 1 (len x)) (scan-let (desugar-dfns (B x))))
     (each x scan)))) ; otherwise recur on subexpressions.


   (, (scan x) (resolve-dfns e) (wev e x)))

   ; this is a huge complicated procedure for "resolving
   ; the addresses" of sets of mutually recursive inner functions
   ; at compile time, so they can be as efficient as toplevel
   ; definitions whenever that's possible. it constructs the
   ; maximum set S of function variables whose definitions
   ; have no free variables not in S. then it arranges for
   ; references to functions in S to get resolved (with
   ; optimizations etc) later, which lets the compiler generate
   ; appropriate code without having a function address yet.
   (resolve-dfns e) (: def (e 'def) val (e 'val)
    ; step one: pass over each inner def for this function and
    ; try to rewrite and evaluate it. if this succeeds, add it
    ; to the known values. otherwise, if the definition is for
    ; a function, collect it into a list.
    (re-ev-dfns dfns) (? dfns (:
     coll (re-ev-dfns (B dfns))
     nom (AA dfns) dfn (wev e (BA dfns))
     (? (quoted? (tset def nom dfn))
         (, (tdel def nom)
            (tset val nom (unquote dfn))
            coll)
        (lambda? dfn) (X (X nom dfn) coll)
        coll)))

    ; step two: now we have a list of function name/definition
    ; pairs. try and compile each one; if it succeeds, add it to
    ; the known values. otherwise collect the name, definition,
    ; and list of free variables into a list.
    (re-co-dfns dfns) (? dfns (:
     coll (re-co-dfns (B dfns))
     nom (AA dfns) dfn (BA dfns)
     co (ltu- e nom (B dfn))
     (?  (twop co) ; if ltu returns a pair there are closure vars; collect the name & def
          (X (X nom (X (A co) dfn)) coll)
         (, (tdel def nom) ; otherwise it's a fully-compiled thread so add it to the known values.
            (tset val nom co)
            coll))))

    ; step three: reject from the list all definitions that
    ; depend on values that won't be available until runtime.
    ; membership in the list is regarded as proof that the
    ; function is available now, so any time a function is
    ; rejected we start over again as it may have been
    ; referenced by a previously accepted function.
    (re-rm-deps dfns) (re-rm-deps-r dfns dfns ())
    (re-rm-deps-chk maybes vs) (?
     (nilp vs) true
     (|| (thas val (: v (A vs)))
         (any maybes (\ j (= (A j) v))))
      (re-rm-deps-chk maybes (B vs)))
    (re-rm-deps-r maybes dfns coll) (?
     (nilp dfns) coll
     (: dfn (A dfns) (?
      (re-rm-deps-chk maybes (AB dfn))
       (re-rm-deps-r maybes (B dfns) (X dfn coll))
      (re-rm-deps (del maybes dfn)))))

    ; step four ( the last one ) : now we have the desired
    ; set of functions. remove them from the scope's local
    ; variables and add them to a list of deferred values, then
    ; recompile them. since we know each of these functions has
    ; no free variables that aren't being deferred, this time
    ; ltu will definitely return a function, which is stored in
    ; the known value set.
    (re-co-dfrs dfns) (?
     (nilp dfns) (tset e 'loc (tkeys def)) ; we're done, set the local variable list
     (: dfn (A dfns)
        nom (A dfn)
      (, (tdel def nom)
         (tset e 'dfr (X nom (e 'dfr)))
         (re-co-dfrs (B dfns))
         (tset val nom (ltu- e nom (B (BB dfn)))))))

    (re-co-dfrs
     (re-rm-deps
      (re-co-dfns
       (re-ev-dfns
        (map (tset e 'loc (tkeys def))
         (\ k (X k (def k)))))))))



  ; second moiety : " analyzing evaluator "
  ; generates code to produce the value of the expression
  ; produced by the first moiety.

  (imm x k) (:
   type-sym (?
    (nump x) 'num (twop x) 'two
    (symp x) 'sym (strp x) 'str
    (tblp x) 'tbl 'hom)
   (emc i-imm (emd x (produces type-sym k))))

  ; these functions are for type checking
  (consumes t k) (? (nilp t) k (:
   type-check (?
    (= t 'two) i-idtwo (= t 'num) i-idno
    (= t 'hom) i-idmo (= t 'tbl) i-idtbl
    (nope'consumes':'what'is'a t))
   (emc type-check k)))

  (produces t k) (:
   (subtype? a b)
    ; currently fixnums are their own thing & every
    ; other kind of data also counts as a hom.
    (|| (= a b) (&& (= b 'hom) (nilp (= a 'num))))
   (con i) (? (= i i-idmo) 'hom (= i i-idno) 'num
              (= i i-idtwo) 'two (= i i-idtbl) 'tbl)
   (pro t h) (: q (con (peek h)) (?
    (nilp q) h
    (subtype? t q) (pro t (seek h 1))
    (nope'produces':'type'error': t'for q)))
   (? (nilp t) k (\ m e (pro t (k m e)))))

  (co-apply fn args k) (:
   ((em-call n k) m e) (: q (k (+ m 2) e) (?
    (= i-ret (peek q))
     (poke i-rec n (seek q 1))
    (poke i-call n q)))
   (foldl
    (co-ev fn (consumes 'hom (em-call (len args) k)))
    args
    (\ k x (co-ev x (emc i-push k)))))



  (co-ev x k) (:
    (co-list x k) (: z (A x) (?
     (= '` z) (imm (? (B x) (AB x)) k)
     (= '? z) (co-cond (B x) k)
     (= ': z) ((? (& (len (B x)) 1) co-let1 co-let0) (B x) k)
     (= '\ z) (co-lambda (B x) k)
     (= ', z) (co-begin (B x) k)
     (co-ap z (B x) k)))

    ; TODO elide pure expressions in medial position
    (co-begin x k) (? x (foldr x k co-ev) (imm 0 k))
    (co-ap fn args k) (:
     q (inliners (? (quoted? fn) (unquote fn) fn))
     (? q (q args k) (co-apply fn args k)))

    (co-let1 x k) (>>= (init x) (last x) k (\ dfns xpn k (?
     (nilp dfns) (co-ev xpn k)
     (co-ev (L (L '\ (L ', (X ': dfns) xpn))) k))))

    ((co-let0 x k) m e) (:
     (nom s) (? (twop s) (nom (A s)) s)
     (loop x k) (? (nilp x) k (:
      cont (loop (BB x) k)
      bind (? (nilp (|| (toplp e) (memq (e 'loc) (A x)))) cont
              (toplp e) (emc i-deftop (emd (X _ns (A x)) cont))
              (emc i-defsl1 (emd (idx (e 'loc) (A x)) cont)))
      v (look e (A x))
      (? (= 'here (A v))
       (imm (B v) bind)
       (co-ev ((e'def) (A x)) bind))))
     (? (nilp x) (imm 0 k)
       ((loop x k) m e)))

    (co-lambda x k) (?
     (nilp x) (imm null-fn k)
      ; FIXME an oddly specific and bad optimization.
      ; it rewrites a lambda expression of the form
      ;   (\ args (: vars vals xpn))
      ; to the form
      ;   (\ args (, (: vars vals) xpn))
      ; which (currently) generates slightly more
      ; efficient code. effectively we should ALWAYS
      ; be doing this, but without variable renaming
      ; this is the only case where we can be sure
      ; it's safe in this case. so, we do it here.
     (letrec? (: args (init x) xpn (last x)))
      (co-lambda (rw-letrec x) k)
     (:

     (nom e k) ; try to pick a name for this function
      (: i (peek k) (?
       (= i i-deftop) (A (peek (seek k 1))) ; maybe we're about to set a global?
       (= i i-defsl1) (at (e 'loc) (peek (seek k 1))))) ; or a local

     (clo-thd e x)
      ((foldl (emc i-take (emd (len x) hom)) x
        (\ k x (co-ev x (emc i-push k)))) 0 e)
      (\ m e (:
       l ((produces 'hom k) (+ m 2) e)
       y (ltu e (nom e l) args xpn)
       (? (twop y) ; this means there's a closure.
           (poke (? (e 'loc) i-encl1 i-encl0)
            (X (B y) (clo-thd e (A y))) l)
          (pokef i-imm (poke y l)))))))

    (co-cond x k) (:
     ; these make functions that do certain side effects
     ; with env stack structures during code generation:
     ; either manipulate the stack or embed the item at the
     ; top into the thread.
     ((pushr s k) m e) (>>= e s (k m e) (\ e s q (A (tset e s (X q (e s))))))
     ((popr  s k) m e) (>>= e s (k m e) (\ e s q (, (tset e s (B (e s))) q)))
     ((peekr s k) m e) (>>= (e s) (k (+ m 1) e) (\ r q (poke (A r) q)))
     (jump-to d k) (: i (peek d) (?
      (= i i-ret) (poke i k)
      (poke i-jump (? (= i i-jump) (peek (seek d 1)) d) k)))
     (loop x k) (:
      (fin m e) (: dst (k (+ m 2) e)
       (jump-to (A (e 's2)) dst))
      (? (nilp (B x)) (co-ev (A x) fin)
       (co-ev (A x) (popr 's1 (emc i-br1 (peekr 's1
        (loop (BB x) (pushr 's1 (co-ev (AB x) fin)))))))))
     (popr 's2 (loop x (pushr 's2 k))))
    ((co-sym x k) m e) (>>= (k (+ m 2) e) (\ k (:
     ; FIXME this mutates the environment by populating closure
     ; variables ; we should have already done that by now!
     r (look e x)
     s (A r) (?
      (= s 'here)
       (pokef i-imm (poke (B r) k))
      (= s 'wait)
       (pokef i-late (poke (X (B r) x) k)) ; TODO subsume runtime type check
      (= e (B r))
       (pokef (? (= s 'arg) i-argn (= s 'loc) i-sl1n (= s 'clo) i-clon)
        (poke (idx (e s) x) k))
      (pokef i-clon (poke (- (len (tset e 'clo (snoc (e 'clo) x))) 1) k))))))

   ((? (twop x) co-list (symp x) co-sym imm) x k))

  (ev x) ((ltu 0 0 0 x) x))))

 (, (ev egg)
    ((ev 'ev) egg)
    (tdel _ns 'inliners 'fuse 'pure 'abelians 'monoids)))

; " drop privileges "
; leave these in for now while we work on the compiler

; 'poke 'seek 'peek 'hom 'hfin '_ns 'macros
; delete instructions
; (ap tdel (X _ns (filter (tkeys _ns) (\ k (= "i-" (ssub (ynom k) 0 2))))))
