#include <stdarg.h>
#include "em.h"

////
/// host embedding
//
// this phase does no optimization
//
#define Push(...) pushs(v, __VA_ARGS__, (ob) 0)
#define Pull(m) ((c1*)getnum(*v->sp++))(v, e, m)


// " compilation environments "
#define arg(x)  ((ob*)(x))[0] // argument variables : a list
#define loc(x)  ((ob*)(x))[1] // local variables : a list
#define clo(x)  ((ob*)(x))[2] // closure variables : a list
#define par(x)  ((ob*)(x))[3] // surrounding scope : tuple or nil
#define name(x) ((ob*)(x))[4] // function name : a symbol or nil
#define asig(x) ((ob*)(x))[5] // arity signature : an integer
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1
#define s1(x)   ((ob*)(x))[6] // extra stacks for branch addresses
#define s2(x)   ((ob*)(x))[7]

enum where { Here, Loc, Arg, Clo, Wait };
#define Co(nom,...) yo nom(em v, ob* e, uintptr_t m, ##__VA_ARGS__)
static bool scan(em, ob*, ob);
typedef Co(c1);
typedef Co(c2, ob);
static c1 xx_yo_, let_yo_bind, em_i, em_i_d, mk_yo;
static c2 xx_yo, var_yo, two_yo, im_yo;

#define Put(x) putnum((ob)(x))

static bool pushss(em v, uintptr_t i, va_list xs) {
  bool _;
  ob x = va_arg(xs, ob);
  return !x ?  Avail >= i || please(v, i) :
    (with(x, _ = pushss(v, i+1, xs)),
     _ && (*--v->sp = x)); }

static bool pushs(em v, ...) {
  va_list xs; bool _; return
    va_start(xs, v),
    _ = pushss(v, 0, xs),
    va_end(xs),
    _; }

static Inline yo ee1(ll *i, yo k) { return (--k)->ll = i, k; }
static Inline yo ee2(ll *i, ob x, yo k) { return ee1(i, ee1((ll*) x, k)); }

// helper functions for lists
static intptr_t lidx(ob l, ob x) {
  for (intptr_t i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

static ob linitp(em v, ob x, ob* d) {
  ob y; return !twop(B(x)) ? (*d = x, nil) :
    (with(x, y = linitp(v, B(x), d)),
     !y ? 0 : pair(v, A(x), y)); }

static ob snoc(em v, ob l, ob x) {
  return !twop(l) ? pair(v, x, l) :
    (with(l, x = snoc(v, B(l), x)),
     !x ? 0 : pair(v, A(l), x)); }

static yo ini_yo(em v, uintptr_t n) {
  yo a = cells(v, n + 2);
  return !a ? 0 :
    (a[n].ll = NULL,
     a[n+1].ll = (ll*) a,
     setw((ob*) a, nil, n),
     a + n); }

static yo tuplr(em v, uintptr_t i, va_list xs) {
  yo k; ob x = va_arg(xs, ob);
  return !x ? ini_yo(v, i) :
    (with(x, k = tuplr(v, i+1, xs)),
     k ? ee1((ll*) x, k) : 0); }

static ob tupl(em v, ...) {
  yo t; va_list xs; return
    va_start(xs, v),
    t = tuplr(v, 0, xs),
    va_end(xs),
    (ob) t; }

static yo imx(em v, ob *e, intptr_t m, ll *i, ob x) {
  return !Push(Put(i), x) ? 0 : em_i_d(v, e, m); }

#define Bind(v, x) if(!((v)=(x)))goto fail
static NoInline ob rw_let_fn(em v, ob x) {
  mm(&x);
  for (ob _; twop(A(x));)
    if (!(_ = snoc(v, BA(x), AB(x)))  ||
        !(_ = pair(v, v->glob[Lamb], _)) ||
        !(_ = pair(v, _, BB(x))) ||
        !(x = pair(v, AA(x), _)))
      return um, 0;
  return um, x; }

static int scan_def(em v, ob *e, ob x) {
  int r; return
    !twop(x) ? 1 : // this is an even case so export all the definitions to the local scope
    !twop(B(x)) ? 0: // this is an odd case so ignore these, they'll be imported after the rewrite
    (mm(&x),
     r = scan_def(v, e, BB(x)),
     r != 1 ? (um, r) :
       !(x = rw_let_fn(v, x)) ||
       !(loc(*e) = pair(v, A(x), loc(*e))) ||
       !scan(v, e, AB(x)) ?
         (um, -1) :
         (um, 1)); }

static bool scan(em v, ob* e, ob x) {
  bool _; return
    !twop(x) ||
    A(x) == v->glob[Lamb] ||
    A(x) == v->glob[Quote] ? 1 :
      A(x) == v->glob[Def] ?
        scan_def(v, e, B(x)) != -1 :
        (with(x, _ = scan(v, e, A(x))),
         _ && scan(v, e, B(x))); }

static ob asign(em v, ob a, intptr_t i, ob*m) {
  ob x; return
    !twop(a) ? (*m = i, a) :
    twop(B(a)) && AB(a) == v->glob[Splat] ?
      (*m = -i-1,
       pair(v, A(a), nil)) :
    (with(a, x = asign(v, B(a), i+1, m)),
     !x ? 0 : pair(v, A(a), x)); }

static Inline ob new_scope(em v, ob*e, ob a, ob n) {
  intptr_t s = 0; return
    !(with(n, a = asign(v, a, 0, &s)), a) ? 0 :
      tupl(v, a, nil, nil, e ? *e : nil, n, putnum(s), nil, nil, (ob) 0); }

static Inline ob comp_body(em v, ob*e, ob x) {
  intptr_t i; return
    !Push(putnum((ob)xx_yo_),
          x,
          putnum((ob)em_i),
          putnum((ob)ret),
          putnum((ob)mk_yo)) ||
    !scan(v, e, v->sp[1]) ||
    !(x = (ob) Pull(4)) ? 0 :
      (x = !(i = llen(loc(*e))) ? x :
        (ob) ee2(locals, putnum(i), (yo) x),
       x = (i = getnum(asig(*e))) > 0 ?
             (ob) ee2(arity, putnum(i), (yo) x) :
           i < 0 ?
             (ob) ee2(vararg, putnum(-i-1), (yo) x) :
           x,
       button(gethom(x))[1].ll = (ll*) x,
       !twop(clo(*e)) ? x : pair(v, clo(*e), x)); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static Inline ob yo_yo_lam(em v, ob* e, ob n, ob l) {
  ob y = nil;
  l = B(l);
  mm(&n), mm(&y), mm(&l);
  if (!(l = twop(l) ? l : pair(v, l, nil)) ||
      !(l = linitp(v, l, &y)) ||
      !(n = pair(v, n, e ? name(*e) : nil)) ||
      !(n = new_scope(v, e, l, n)) ||
      !(l = comp_body(v, &n, A(y))))
    return um, um, um, 0;
  return um, um, um, l; }

static Inline ob yo_yo_clo(em v, ob*e, ob arg, ob seq) {
  intptr_t i = llen(arg);
  mm(&arg), mm(&seq);
  if (!Push(Put(em_i_d), Put(take), putnum(i), Put(mk_yo)))
    return um, um, 0;

  for (;twop(arg);arg = B(arg))
    if (!Push(Put(xx_yo_), A(arg), Put(em_i), Put(push)))
      return um, um, 0;

  if (!(arg = (ob) Pull(0))) return um, um, 0;

  return um, um, pair(v, seq, arg); }

static Co(yo_yo, ob x) {
 ll* j = imm;
 ob k, nom = *v->sp == Put(let_yo_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = (ob) Pull(m+2)));
 if (!k) return 0;
 mm(&k);
 if (twop(x = yo_yo_lam(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = yo_yo_clo(v, e, A(x), B(x));
 um;
 return !x ? 0 : ee2(j, x, (yo) k); }

static Co(im_yo, ob x) {
  return !(x = Push(Put(imm), x)) ? 0 : em_i_d(v, e, m); }

static Co(let_yo_bind) {
  ob _ = *v->sp++;
  return e ? imx(v, e, m, loc_, putnum(lidx(loc(*e), _))) :
             imx(v, e, m, tbind, _); }

static bool let_yo_r(em v, ob*e, ob x) {
  bool _; return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = let_yo_r(v, e, BB(x))), _) &&
     Push(Put(xx_yo_), AB(x), Put(let_yo_bind), A(x))); }

// syntactic sugar for define
static Inline ob def_sug(em v, ob x) {
  ob _ = nil; return
    (with(_, x = linitp(v, x, &_)), x) &&
    (x = pair(v, x, _)) &&
    (x = pair(v, v->glob[Seq], x)) &&
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->glob[Lamb], x)) ?
    pair(v, x, nil) :
    0; }

static Co(let_yo, ob x) { return
  !twop(B(x)) ? im_yo(v, e, m, nil) :
  llen(B(x)) % 2 ?
    (x = def_sug(v, x)) ? xx_yo(v, e, m, x) : 0 :
  (x = let_yo_r(v, e, B(x))) ? Pull(m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
static Co(if_yo_pre) { ob x; return
  (x = (ob) Pull(m)) && (x = pair(v, x, s2(*e))) ?
    (s2(*e) = x, (yo) A(x)) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
static Co(if_yo_pre_con) {
  yo k, x = Pull(m + 2);
  return !x ? 0 :
    (k = (yo) A(s2(*e)),
     k->ll == ret ?
       ee1(ret, x) :
       ee2(jump, (ob) k, x)); }

// after generating a branch store its address
// in stack 1
static Co(if_yo_post_con) {
  ob x; return
    (x = (ob) Pull(m)) &&
    (x = pair(v, x, s1(*e))) ?
      (s1(*e) = x, (yo) A(x)) :
      0; }

// before generating an antecedent emit a branch to
// the top of stack 1
static Co(if_yo_pre_ant) {
  yo x = Pull(m+2);
  return !x ? 0 :
    (x = ee2(branch, A(s1(*e)), x),
     s1(*e) = B(s1(*e)),
     x); }

static bool if_yo_loop(em v, ob*e, ob x) {
  bool _; return 
    x = twop(x) ? x : pair(v, nil, nil),
    !x ? 0 : !twop(B(x)) ?
      Push(putnum((ob)xx_yo_),
                  A(x),
                  putnum((ob)if_yo_pre_con)) :
    !(with(x, _ = Push(putnum((ob)if_yo_post_con),
                      putnum((ob)xx_yo_),
                      AB(x),
                      putnum((ob)if_yo_pre_con))), _) ||
    !(with(x, _ = if_yo_loop(v, e, BB(x))), _) ? 0 :

    Push(putnum((ob)xx_yo_),
         A(x),
         putnum((ob)if_yo_pre_ant)); }

static Co(if_yo, ob x) { bool _; yo k; return
  with(x, _ = Push(putnum((ob)if_yo_pre))),
  _ && if_yo_loop(v, e, B(x)) && (k = Pull(m)) ?
    (s2(*e) =  B(s2(*e)), k) :
    0; }

static Co(em_call) {
  ob a = *v->sp++;
  yo k = Pull(m + 2);
  return !k ? 0 : ee2(k->ll == ret ? rec : call, a, k); }

static ob lookup_mod(em v, ob x) {
  return tbl_get(v, v->glob[Topl], x); }

static ob lookup_lex(em v, ob e, ob y) {
  ob q; return
    nilp(e) ?
      (q = lookup_mod(v, y)) ?
        pair(v, putnum(Here), q) :
        pair(v, putnum(Wait), v->glob[Topl]) :
    lidx(loc(e), y) > -1 ? pair(v, putnum(Loc), e) :
    lidx(arg(e), y) > -1 ? pair(v, putnum(Arg), e) :
    lidx(clo(e), y) > -1 ? pair(v, putnum(Clo), e) :
    lookup_lex(v, par(e), y); }

static Co(var_yo, ob x) { ob y, q; return
  with(x, q = lookup_lex(v, e ? *e:nil, x)),
  !q ? 0 : (y = A(q)) == putnum(Here) ?
     im_yo(v, e, m, B(q)) :
     y == putnum(Wait) ? (
      (x = pair(v, B(q), x)) &&
      (with(x, y = (ob) Pull(m+2)), y) &&
      (with(y, x = pair(v, putnum(sizeof(ob)), x)), x) ?
        ee2(lbind, x, (yo) y) :
        0 ) :
     B(q) == *e ?
        y == putnum(Loc) ?
          imx(v, e, m, loc, putnum(lidx(loc(*e), x))):
        y == putnum(Arg) ?
          imx(v, e, m, arg, putnum(lidx(arg(*e), x))) :
        imx(v, e, m, clo, putnum(lidx(clo(*e), x))) :
        (y = llen(clo(*e)),
        with(x, q = snoc(v, clo(*e), x)),
        !q ? 0 : (clo(*e) = q,
                  imx(v, e, m, clo, putnum(y)))); }

static Co(xx_yo_) { return xx_yo(v, e, m, *v->sp++); }
static Co(xx_yo, ob x) { return
  (symp(x) ? var_yo : twop(x) ? two_yo : im_yo)(v, e, m, x); }

static Co(ap_yo, ob fun, ob args) {
  mm(&args);
  if (!Push(Put(xx_yo_), fun,
            Put(em_i), Put(idH),
            Put(em_call), putnum(llen(args))))
    return um, NULL;
  for (;twop(args);args=B(args))
    if (!Push(Put(xx_yo_), A(args),
              Put(em_i), Put(push)))
      return um, NULL;

  return um, Pull(m); }

static bool seq_yo_loop(em v, ob*e, ob x) {
  bool _; return !twop(x) ? 1 :
    (with(x, _ = seq_yo_loop(v, e, B(x))), _) &&
    Push(Put(xx_yo_), A(x)); }

static Co(two_yo, ob x) { ob z = A(x); return 
  z == v->glob[Cond] ? if_yo(v, e, m, x) :
  z == v->glob[Def]  ? let_yo(v, e, m, x) :
  z == v->glob[Lamb] ? yo_yo(v, e, m, x) :

  z == v->glob[Seq]  ?
    (twop(x = B(x)) || (x = pair(v, x, nil))) &&
    (x = seq_yo_loop(v, e, x)) ?
      Pull(m) :
      0 :

  z == v->glob[Quote] ?
    (x = twop(x = B(x)) ? A(x) : x,
     im_yo(v, e, m, x)) :

  ap_yo(v, e, m, A(x), B(x)); }

static Co(em_i) {
  ll* i = (ll*) getnum(*v->sp++);
  yo k = Pull(m+1);
  return !k ? 0 : ee1(i, k); }

static Co(em_i_d) {
  ll* i = (ll*) getnum(*v->sp++);
  ob x = *v->sp++;
  yo k; return
    with(x, k = Pull(m+2)),
    !k ? 0 : ee2(i, x, k); }

static Co(mk_yo) {
  yo k = ini_yo(v, m+1);
  return !k ? 0 : ee1((ll*)(e ? name(*e) : nil), k); }

static ob apply(em, ob, ob) NoInline;
ob eval(em v, ob x) {
  ob args = pair(v, x, nil);
  return !args ? 0 :
  (x = homp(v->glob[Eval]) ?
     v->glob[Eval] : tbl_get(v, v->glob[Topl], v->glob[Eval]),
   apply(v, x, args)); }

// return to C
static Ll(yield) { Pack(); return xp; }

static NoInline ob apply(em v, ob f, ob x) {
  yo h; return
    !Push(f, x) || !(h = cells(v, 5)) ? 0 :
      (h[0].ll = call,
       h[1].ll = (ll*) putnum(2),
       h[2].ll = yield,
       h[3].ll = NULL,
       h[4].ll = (ll*) h,
       x = tbl_get(v, v->glob[Topl], v->glob[Apply]),
       call(v, h, (ob*) v->fp, v->sp, v->hp, x)); }

// instructions used by the compiler
Ll(hom_u) {
  Arity(1);
  ob x = *Argv;
  TypeCheck(x, Num);
  intptr_t len = getnum(x) + 2;
  Have(len);
  yo k = (yo) hp;
  return hp += len,
         setw((ob*) k, nil, len),
         k[len-1].ll = (ll*) k,
         k[len-2].ll = NULL,
         ApC(ret, (ob) (k+len-2)); }

Ll(hfin_u) {
  Arity(1);
  yo k = (yo) *Argv;
  TypeCheck((ob) k, Hom);
  return button(k)[1].ll = (ll*) k,
         ApC(ret, (ob) k); }

Ll(emx) {
  yo k = (yo) *sp++ - 1;
  return k->ll = (ll*) xp,
         ApN(1, (ob) k); }

Ll(emi) {
  yo k = (yo) *sp++ - 1;
  return k->ll = (ll*) getnum(xp),
         ApN(1, (ob) k); }

Ll(emx_u) {
  Arity(2);
  ob x = Argv[0];
  yo k = (yo) Argv[1];
  TypeCheck((ob) k, Hom);
  return (--k)->ll = (ll*) x,
         ApC(ret, (ob) k); }

Ll(emi_u) {
  Arity(2);
  ob n = Argv[0];
  yo k = (yo) Argv[1];
  TypeCheck(n, Num);
  TypeCheck((ob) k, Hom);
  return (--k)->ll = (ll*) getnum(n),
         ApC(ret, (ob) k); }

Ll(hgeti_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, Put(gethom(*Argv)->ll)); }

Ll(hgetx_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, (ob) gethom(*Argv)->ll); }

Ll(hseek_u) {
  Arity(2);
  TypeCheck(Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  return ApC(ret, puthom(gethom(Argv[0]) + getnum(Argv[1]))); }

ob analyze(em v, ob x) {
  with(x, Push(Put(em_i), Put(ret), Put(mk_yo)));
  return (ob) xx_yo(v, NULL, 0, x); }

Ll(ev_u) {
  Arity(1);
  return
    homp(v->glob[Eval]) ?
      ApY((yo) v->glob[Eval], xp) :
    (Pack(),
     (v->ip = (yo) analyze(v, *Argv)) ?
       (Unpack(), ApY(ip, xp)) :
       0); }

Ll(bootstrap) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return
    v->glob[Eval] = xp = *Argv, // FIXME neither intern nor tbl_set will allocate if ev is already interned / defined
    tbl_set(v, v->glob[Topl], interns(v, "ev"), xp),
    ApC(ret, xp); }

Ll(hnom_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, homnom(v, *Argv)); }

ob sequence(em v, ob a, ob b) {
  yo h;
  with(a, with(b, h = cells(v, 8)));
  return !h ? 0 :
    (h[0].ll = imm,
     h[1].ll = (ll*) a,
     h[2].ll = call,
     h[3].ll = (ll*) N0,
     h[4].ll = jump,
     h[5].ll = (ll*) b,
     h[6].ll = NULL,
     h[7].ll = (ll*) h,
     (ob) h); }
