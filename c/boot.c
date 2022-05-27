#include <stdarg.h>
#include "la.h"

////
/// host embedding
//
// this phase does no optimization
//

#define Co(nom,...) mo nom(la v, ob *e, N m, ##__VA_ARGS__)
typedef Co(c1);
typedef Co(c2, ob);
static c1 em_i, em_i_d, mk_mo;
static c2 xx_mo;

mo ana(ph v, ob x, ob k) {
  bool _;
  with(x, _ = nump(k) ?
    Push(putnum(em_i), k, putnum(mk_mo)) :
    Push(putnum(em_i_d), putnum(jump), k, putnum(mk_mo)));
  return !_ ? 0 : xx_mo(v, NULL, 0, x); }

static bool pushss(em v, uintptr_t i, va_list xs) {
  bool _;
  ob x = va_arg(xs, ob);
  return !x ?  Avail >= i || please(v, i) :
    (with(x, _ = pushss(v, i+1, xs)),
     _ && (*--v->sp = x)); }

bool pushs(em v, ...) {
  va_list xs; bool _; return
    va_start(xs, v),
    _ = pushss(v, 0, xs),
    va_end(xs),
    _; }

static mo ini_mo(em v, uintptr_t n) {
  mo a = cells(v, n + 2);
  return !a ? 0 :
    (a[n].ll = NULL,
     a[n+1].ll = (ll*) a,
     setw((ob*) a, nil, n),
     a + n); }

static c1 xx_mo_, let_mo_bind;
static bool scan(em, ob*, ob);

static mo imx(em v, ob *e, intptr_t m, ll *i, ob x) {
  return !Push(putnum(i), x) ? 0 : em_i_d(v, e, m); }

#define Bind(v, x) if(!((v)=(x)))goto fail
static NoInline ob rw_let_fn(em v, ob x) {
  mm(&x);
  for (ob _; twop(A(x));)
    if (!(_ = snoc(v, BA(x), AB(x)))  ||
        !(_ = pair(v, v->lex[Lamb], _)) ||
        !(_ = pair(v, _, BB(x))) ||
        !(x = pair(v, AA(x), _)))
      return um, 0;
  return um, x; }


static ob asign(em v, ob a, Z i, ob*m) {
  ob x; return
    !twop(a) ? (*m = i, a) :
    twop(B(a)) && AB(a) == v->lex[Splat] ?
      (*m = -i-1,
       pair(v, A(a), nil)) :
    (with(a, x = asign(v, B(a), i+1, m)),
     !x ? 0 : pair(v, A(a), x)); }

// " compilation environments "
#define arg(x)  ((ob*)(x))[0] // argument variables : a list
#define loc(x)  ((ob*)(x))[1] // local variables : a list
#define clo(x)  ((ob*)(x))[2] // closure variables : a list
#define par(x)  ((ob*)(x))[3] // surrounding scope : tuple or nil
#define name(x) ((ob*)(x))[4] // function name : a symbol or nil
#define asig(x) ((ob*)(x))[5] // arity signature : an integer
#define s1(x)   ((ob*)(x))[6] // stacks for branch addresses
#define s2(x)   ((ob*)(x))[7]
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1

static Inline ob new_scope(em v, ob *e, ob a, ob n) {
  Z *x, s = 0; return
    !(with(n, a = asign(v, a, 0, &s)), a) ||
    !(with(n, with(a, x = cells(v, 10))), x) ? 0 :
    (arg(x) = a,
     loc(x) = clo(x) = s1(x) = s2(x) = nil,
     par(x) = e ? *e : nil,
     name(x) = n,
     asig(x) = putnum(s),
     x[8] = 0,
     x[9] = (ob) x); }

static c2 var_mo, two_mo, im_mo;
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
    A(x) == v->lex[Lamb] ||
    A(x) == v->lex[Quote] ? 1 :
      A(x) == v->lex[Def] ?
        scan_def(v, e, B(x)) != -1 :
        (with(x, _ = scan(v, e, A(x))),
         _ && scan(v, e, B(x))); }

static Inline mo pb1(host *i, mo k) {
  return (--k)->ll = i, k; }
static Inline mo pb2(host *i, ob x, mo k) {
  return pb1(i, pb1((host *) x, k)); }

#define Pull(m) ((c1*)getnum(*v->sp++))(v, e, m)
static Inline ob comp_body(em v, ob*e, ob x) {
  intptr_t i; return
    !Push(putnum(xx_mo_),
          x,
          putnum(em_i),
          putnum(ret),
          putnum(mk_mo)) ||
    !scan(v, e, v->sp[1]) ||
    !(x = (ob) Pull(4)) ? 0 :
      (x = !(i = llen(loc(*e))) ? x :
        (ob) pb2(locals, putnum(i), (mo) x),
       x = (i = getnum(asig(*e))) > 0 ?
             (ob) pb2(arity, putnum(i), (mo) x) :
           i < 0 ?
             (ob) pb2(vararg, putnum(-i-1), (mo) x) :
           x,
       button(gethom(x))[1].ll = (ll*) x,
       !twop(clo(*e)) ? x : pair(v, clo(*e), x)); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static Inline ob mo_mo_lam(la v, ob* e, ob n, ob l) {
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

static Inline ob mo_mo_clo(em v, ob*e, ob arg, ob seq) {
  Z i = llen(arg);
  mm(&arg), mm(&seq);
  if (!Push(putnum(em_i_d), putnum(take), putnum(i), putnum(mk_mo)))
    return um, um, 0;

  for (;twop(arg);arg = B(arg))
    if (!Push(putnum(xx_mo_), A(arg), putnum(em_i), putnum(push)))
      return um, um, 0;

  if (!(arg = (ob) Pull(0))) return um, um, 0;

  return um, um, pair(v, seq, arg); }

static Co(mo_mo, ob x) {
 ll* j = imm;
 ob k, nom = *v->sp == putnum(let_mo_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = (ob) Pull(m+2)));
 if (!k) return 0;
 mm(&k);
 if (twop(x = mo_mo_lam(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = mo_mo_clo(v, e, A(x), B(x));
 um;
 return !x ? 0 : pb2(j, x, (mo) k); }

static Co(im_mo, ob x) {
  return !(x = Push(putnum(imm), x)) ? 0 : em_i_d(v, e, m); }

static Co(let_mo_bind) {
  ob _ = *v->sp++;
  return e ? imx(v, e, m, loc_, putnum(lidx(loc(*e), _))) :
    (_ = pair(v, A(v->wns), _)) ? imx(v, e, m, tbind, _) :
    0; }

static bool let_mo_r(em v, ob*e, ob x) {
  bool _; return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = let_mo_r(v, e, BB(x))), _) &&
     Push(putnum(xx_mo_), AB(x), putnum(let_mo_bind), A(x))); }

// syntactic sugar for define
static Inline ob def_sug(em v, ob x) {
  ob _ = nil; return
    (with(_, x = linitp(v, x, &_)), x) &&
    (x = pair(v, x, _)) &&
    (x = pair(v, v->lex[Seq], x)) &&
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->lex[Lamb], x)) ?
    pair(v, x, nil) :
    0; }

static Co(let_mo, ob x) { return
  !twop(B(x)) ? im_mo(v, e, m, nil) :
  llen(B(x)) % 2 ?
    (x = def_sug(v, x)) ? xx_mo(v, e, m, x) : 0 :
  (x = let_mo_r(v, e, B(x))) ? Pull(m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
static Co(if_mo_pre) { ob x; return
  (x = (ob) Pull(m)) && (x = pair(v, x, s2(*e))) ?
    (s2(*e) = x, (mo) A(x)) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
static Co(if_mo_pre_con) {
  mo k, x = Pull(m + 2);
  return !x ? 0 :
    (k = (mo) A(s2(*e)),
     k->ll == ret ?
       pb1(ret, x) :
       pb2(jump, (ob) k, x)); }

// after generating a branch store its address
// in stack 1
static Co(if_mo_post_con) {
  ob x; return
    (x = (ob) Pull(m)) &&
    (x = pair(v, x, s1(*e))) ?
      (s1(*e) = x, (mo) A(x)) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
static Co(if_mo_pre_ant) {
  mo x = Pull(m+2);
  return !x ? 0 :
    (x = pb2(branch, A(s1(*e)), x),
     s1(*e) = B(s1(*e)),
     x); }

static bool if_mo_loop(em v, ob*e, ob x) {
  bool _; return
    x = twop(x) ? x : pair(v, nil, nil),
    !x ? 0 : !twop(B(x)) ?
      Push(putnum(xx_mo_), A(x), putnum(if_mo_pre_con)) :
    !(with(x, _ = Push(putnum(if_mo_post_con),
                       putnum(xx_mo_),
                       AB(x),
                       putnum(if_mo_pre_con))), _) ||
    !(with(x, _ = if_mo_loop(v, e, BB(x))), _) ? 0 :

    Push(putnum(xx_mo_),
         A(x),
         putnum(if_mo_pre_ant)); }

static Co(if_mo, ob x) { bool _; mo k; return
  with(x, _ = Push(putnum(if_mo_pre))),
  _ && if_mo_loop(v, e, B(x)) && (k = Pull(m)) ?
    (s2(*e) =  B(s2(*e)), k) :
    0; }

static Co(em_call) {
  ob a = *v->sp++;
  mo k = Pull(m + 2);
  return !k ? 0 : pb2(k->ll == ret ? rec : call, a, k); }

enum where { Here, Loc, Arg, Clo, Wait };
static ob ls_lex(em v, ob e, ob y) {
  ob q; return
    nilp(e) ?
      (q = refer(v, y)) ?
        pair(v, putnum(Here), q) :
        pair(v, putnum(Wait), A(v->wns)) :
    lidx(loc(e), y) > -1 ? pair(v, putnum(Loc), e) :
    lidx(arg(e), y) > -1 ? pair(v, putnum(Arg), e) :
    lidx(clo(e), y) > -1 ? pair(v, putnum(Clo), e) :
    ls_lex(v, par(e), y); }

static Co(var_mo, ob x) { ob y, q; return
  (with(x, q = ls_lex(v, e ? *e:nil, x)), !q) ? 0 :
    (y = A(q)) == putnum(Here) ?
      im_mo(v, e, m, B(q)) :
    y == putnum(Wait) ?
      ((x = pair(v, B(q), x)) &&
       (with(x, y = (ob) Pull(m+2)), y) &&
       (with(y, x = pair(v, putnum(sizeof(ob)), x)), x) ?
         pb2(rslv, x, (mo) y) :
         0) :
    B(q) == *e ?
      y == putnum(Loc) ?
        imx(v, e, m, loc, putnum(lidx(loc(*e), x))) :
      y == putnum(Arg) ?
        imx(v, e, m, arg, putnum(lidx(arg(*e), x))) :
      imx(v, e, m, clo, putnum(lidx(clo(*e), x))) :
    (y = llen(clo(*e)),
     with(x, q = snoc(v, clo(*e), x)),
     !q ? 0 : (clo(*e) = q, imx(v, e, m, clo, putnum(y)))); }

static Co(xx_mo_) { return xx_mo(v, e, m, *v->sp++); }
static Co(xx_mo, ob x) { return
  (symp(x) ? var_mo : twop(x) ? two_mo : im_mo)(v, e, m, x); }

static Co(ap_mo, ob fun, ob args) {
  mm(&args);
  if (!Push(putnum(xx_mo_),
            fun,
            putnum(em_i),
            putnum(idH),
            putnum(em_call),
            putnum(llen(args))))
    return um, NULL;
  for (; twop(args); args = B(args))
    if (!Push(putnum(xx_mo_),
              A(args),
              putnum(em_i),
              putnum(push)))
      return um, NULL;
  return um, Pull(m); }

static bool seq_mo_loop(em v, ob*e, ob x) {
  bool _; return !twop(x) ? 1 :
    (with(x, _ = seq_mo_loop(v, e, B(x))), _) &&
    Push(putnum(xx_mo_), A(x)); }

static Co(two_mo, ob x) { ob z = A(x); return
  z == v->lex[Cond] ? if_mo(v, e, m, x) :
  z == v->lex[Def]  ? let_mo(v, e, m, x) :
  z == v->lex[Lamb] ? mo_mo(v, e, m, x) :
  z == v->lex[Seq]  ?
    (twop(x = B(x)) || (x = pair(v, x, nil))) &&
    (x = seq_mo_loop(v, e, x)) ? Pull(m) : 0 :
  z == v->lex[Quote] ?
    im_mo(v, e, m, twop(B(x)) ? AB(x) : B(x)) :
  ap_mo(v, e, m, A(x), B(x)); }

static Co(em_i) {
  ll* i = (ll*) getnum(*v->sp++);
  mo k = Pull(m+1);
  return !k ? 0 : pb1(i, k); }

static Co(em_i_d) {
  ll* i = (ll*) getnum(*v->sp++);
  ob x = *v->sp++;
  mo k; return
    with(x, k = Pull(m+2)),
    !k ? 0 : pb2(i, x, k); }

static Co(mk_mo) {
  mo k = ini_mo(v, m+1);
  return !k ? 0 : pb1((ll*)(e ? name(*e) : nil), k); }
