#include "la.h"
#include "vm.h"
#include <stdarg.h>


// bootstrap eval interpreter function
Vm(ev_u) {
  ArityCheck(1);
  // check to see if ev has been overridden in the
  // toplevel namespace and if so call that. this way
  // ev calls compiled pre-bootstrap will use the
  // bootstrapped compiler, which is what we want?
  // seems kind of strange to need this ...
  xp = refer(v, v->lex[Eval]);
  if (xp && homp(xp) && gethom(xp)->ll != ev_u)
    return ApY((mo) xp, nil);
  Pack();
  mo y = ana(v, *fp->argv, putZ(ret));
  if (!y) return 0;
  Unpack();
  return ApY(y, xp); }

// bootstrap thread compiler
//
typedef struct env {
  ob arg, loc, clo, par, name, asig, s1, s2; } *env;

static Inline mo pull(la v, ob *e, size_t m) {
  return ((mo (*)(la, ob*, size_t)) getZ(*v->sp++))(v, e, m); }

static Inline mo pb1(vm *i, mo k) {
  return (--k)->ll = i, k; }

static Inline mo pb2(vm *i, ob x, mo k) {
  return pb1(i, pb1((vm*) x, k)); }

// " compilation environments "
#define arg(x)  ((env)(x))->arg // argument variables : a list
#define loc(x)  ((env)(x))->loc // local variables : a list
#define clo(x)  ((env)(x))->clo // closure variables : a list
#define par(x)  ((env)(x))->par // surrounding scope : tuple or nil
#define name(x) ((env)(x))->name // function name : a symbol or nil
#define asig(x) ((env)(x))->asig // arity signature : an integer
#define s1(x)   ((env)(x))->s1 // stacks for branch addresses
#define s2(x)   ((env)(x))->s2
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1

static bool scan(la, ob*, ob);

static mo
  i1d0(la, ob*, size_t),
  i1d1(la, ob*, size_t),
  co_ini(la, ob*, size_t),
  co__(la, ob*, size_t),
  co_ys(la, ob*, size_t),
  co_var(la, ob*, size_t, ob),
  co_2(la, ob*, size_t, ob),
  co_x(la, ob*, size_t, ob);

// pull back over an expression
mo ana(la v, ob x, ob k) {
  // k can be a continuation or an instruction pointer
  bool ok = nump(k) ?
    Push(putZ(co__), x, putZ(i1d0), k, putZ(co_ini)) :
    Push(putZ(co__), x, putZ(i1d1), putZ(jump), k, putZ(co_ini));
  return ok ? pull(v, 0, 0) : 0; }

#define Co(nom,...) static mo nom(la v, ob *e, size_t m, ##__VA_ARGS__)

static mo imx(la v, ob *e, intptr_t m, vm *i, ob x) {
  return Push(putnum(i), x) ? i1d1(v, e, m) : 0; }

static ob snoc(la v, ob l, ob x) {
  return !twop(l) ? pair(v, x, l) :
    (with(l, x = snoc(v, B(l), x)),
     x ? pair(v, A(l), x) : 0); }

#define Bind(v, x) if(!((v)=(x)))goto fail
static NoInline ob rw_let_fn(la v, ob x) {
  mm(&x);
  for (ob _; twop(A(x));)
    if (!(_ = snoc(v, BA(x), AB(x)))  ||
        !(_ = pair(v, v->lex[Lamb], _)) ||
        !(_ = pair(v, _, BB(x))) ||
        !(x = pair(v, AA(x), _)))
      return um, 0;
  return um, x; }


static ob asign(la v, ob a, intptr_t i, ob *m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == v->lex[Splat])
    return *m = -i-1, pair(v, A(a), nil);
  with(a, x = asign(v, B(a), i+1, m));
  return x ? pair(v, A(a), x) : 0; }

static Inline ob new_scope(la v, ob *e, ob a, ob n) {
  intptr_t *x, s = 0;
  with(n,
    a = asign(v, a, 0, &s),
    x = !a ? 0 : (with(a, x = cells(v, 10)), x));
  return !x ? 0 :
    (arg(x) = a,
     loc(x) = clo(x) = s1(x) = s2(x) = nil,
     par(x) = e ? *e : nil,
     name(x) = n,
     asig(x) = putZ(s),
     x[8] = 0,
     x[9] = (ob) x); }

static int scan_def(la v, ob *e, ob x) {
  int r;
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  with(x,
    r = scan_def(v, e, BB(x)),
    r = r != 1 ? r :
      !(x = rw_let_fn(v, x)) ||
      !(loc(*e) = pair(v, A(x), loc(*e))) ||
      !scan(v, e, AB(x)) ? -1 : 1);
  return r; }

static bool scan(la v, ob* e, ob x) {
  bool _;
  if (!twop(x) || A(x) == v->lex[Lamb] || A(x) == v->lex[Quote])
    return 1;
  if (A(x) == v->lex[Def]) return scan_def(v, e, B(x)) != -1;
  with(x, _ = scan(v, e, A(x)));
  return _ && scan(v, e, B(x)); }

static Inline ob comp_body(la v, ob*e, ob x) {
  intptr_t i;
  if (!Push(putZ(co__), x, putZ(i1d0), putZ(ret), putZ(co_ini)) ||
      !scan(v, e, v->sp[1]) ||
      !(x = (ob) pull(v, e, 4)))
    return 0;

  x = !(i = llen(loc(*e))) ? x :
   (ob) pb2(locals, putZ(i), (mo) x);
  x = (i = getZ(asig(*e))) > 0 ?
        (ob) pb2(arity, putZ(i), (mo) x) :
      i < 0 ?
        (ob) pb2(vararg, putZ(-i-1), (mo) x) :
      x;
  button(gethom(x))[1].ll = (vm*) x;
  return !twop(clo(*e)) ? x : pair(v, clo(*e), x); }

static ob linitp(la v, ob x, ob* d) {
  ob y;
  if (!twop(B(x))) return *d = x, nil;
  with(x, y = linitp(v, B(x), d));
  return y ? pair(v, A(x), y) : 0; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static Inline ob co_tl(la v, ob* e, ob n, ob l) {
  ob y = nil;
  l = B(l);
  mm(&n), mm(&y), mm(&l);
  if (
    !(l = twop(l) ? l : pair(v, l, nil)) ||
    !(l = linitp(v, l, &y)) ||
    !(n = pair(v, n, e ? name(*e) : nil)) ||
    !(n = new_scope(v, e, l, n)) ||
    !(l = comp_body(v, &n, A(y))))
      return um, um, um, 0;
  return um, um, um, l; }

static Inline ob co_t_clo(la v, ob*e, ob arg, ob seq) {
  intptr_t i = llen(arg);
  mm(&arg), mm(&seq);
  if (!Push(putZ(i1d1), putZ(take), putZ(i), putZ(co_ini)))
    return um, um, 0;

  for (; twop(arg); arg = B(arg))
    if (!Push(putZ(co__), A(arg), putZ(i1d0), putZ(push)))
      return um, um, 0;

  if (!(arg = (ob) pull(v, e, 0))) return um, um, 0;

  return um, um, pair(v, seq, arg); }

Co(co_t, ob x) {
 vm* j = imm;
 ob k, nom = *v->sp == putnum(co_ys) ? v->sp[1] : nil;
 with(nom, with(x, k = (ob) pull(v, e, m+2)));
 if (!k) return 0;
 mm(&k);
 if (twop(x = co_tl(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = co_t_clo(v, e, A(x), B(x));
 um;
 return !x ? 0 : pb2(j, x, (mo) k); }

Co(co_ys) {
  ob _ = *v->sp++;
  if (e) return imx(v, e, m, loc_, putZ(lidx(loc(*e), _)));
  _ = pair(v, A(v->wns), _);
  return _ ? imx(v, e, m, tbind, _) : 0; }

static bool dty_r(la v, ob*e, ob x) {
  bool _;
  return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = dty_r(v, e, BB(x))), _) &&
     Push(putnum(co__), AB(x), putnum(co_ys), A(x))); }

// syntactic sugar for define
static bool def_sug(la v, ob x) {
  ob _ = nil;
  with(_, x = linitp(v, x, &_));
  return x &&
    (x = pair(v, x, _)) &&
    (x = pair(v, v->lex[Seq], x)) &&
    (x = pair(v, x, nil)) &&
    (x = pair(v, v->lex[Lamb], x)) ?
      Push(putnum(co__), pair(v, x, nil)) :
      0; }

Co(dty, ob x) {
  if (!twop(B(x))) return co_x(v, e, m, nil);
  x = llen(B(x)) % 2 ? def_sug(v, x) : dty_r(v, e, B(x));
  return x ? pull(v, e, m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
Co(co_p_pre) {
  ob x = (ob) pull(v, e, m);
  x = x ? pair(v, x, s2(*e)) : x;
  if (!x) return 0;
  s2(*e) = x;
  return (mo) A(x); }

// before generating a branch emit a jump to
// the top of stack 2
Co(co_p_pre_con) {
  mo k, x = pull(v, e, m + 2);
  if (!x) return 0;
  k = (mo) A(s2(*e));
  return k->ll == ret ? pb1(ret, x) : pb2(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
Co(co_p_post_con) {
  ob x = (ob) pull(v, e, m);
  x = x ? pair(v, x, s1(*e)) : x;
  if (!x) return 0;
  s1(*e) = x;
  return (mo) A(x); }

// before generating an antecedent emit a branch to
// the top of stack 1
Co(co_p_pre_ant) {
  mo x = pull(v, e, m+2);
  if (!x) return 0;
  x = pb2(branch, A(s1(*e)), x);
  s1(*e) = B(s1(*e));
  return x; }

static bool co_p_loop(la v, ob*e, ob x) {
  bool _;
  x = twop(x) ? x : pair(v, nil, nil);
  if (!x) return 0;
  if (!twop(B(x))) return
    Push(putZ(co__), A(x), putZ(co_p_pre_con));
  with(x,
    _ = Push(putZ(co_p_post_con), putZ(co__),
             AB(x), putZ(co_p_pre_con)),
    _ = _ ? co_p_loop(v, e, BB(x)) : _);
  return _ ? Push(putZ(co__), A(x), putZ(co_p_pre_ant)) : 0; }

Co(co_p, ob x) {
  bool _;
  mo pf;
  with(x, _ = Push(putZ(co_p_pre)));
  _ = _ ? co_p_loop(v, e, B(x)) : _;
  if (!_ || !(pf = pull(v, e, m))) return 0;
  s2(*e) = B(s2(*e));
  return pf; }

Co(em_call) {
  ob ary = *v->sp++;
  mo pf = pull(v, e, m + 2);
  if (!pf) return 0;
  vm *i = pf->ll == ret ? rec : call;
  return pb2(i, ary, pf); }

enum where { Here, Loc, Arg, Clo, Wait };

static ob ls_lex(la v, ob e, ob y) {
  ob q; return
    nilp(e) ?
      (q = refer(v, y)) ? pair(v, putZ(Here), q) :
                          pair(v, putZ(Wait), A(v->wns)) :
    lidx(loc(e), y) >= 0 ? pair(v, putZ(Loc), e) :
    lidx(arg(e), y) >= 0 ? pair(v, putZ(Arg), e) :
    lidx(clo(e), y) >= 0 ? pair(v, putZ(Clo), e) :
    ls_lex(v, par(e), y); }

Co(co_var, ob x) {
  ob y, q;
  with(x, q = ls_lex(v, e ? *e : nil, x));
  if (!q) return 0;
  y = A(q);
  if (y == putZ(Here)) return co_x(v, e, m, B(q)) ;
  if (y == putZ(Wait)) return
    (x = pair(v, B(q), x)) &&
    (with(x, y = (ob) pull(v, e, m+2)), y) &&
    (with(y, x = pair(v, putZ(sizeof(ob)), x)), x) ?
      pb2(latebind, x, (mo) y) : 0;

  if (B(q) == *e) return
    y == putZ(Loc) ?
      imx(v, e, m, loc, putZ(lidx(loc(*e), x))) :
    y == putZ(Arg) ?
      imx(v, e, m, arg, putZ(lidx(arg(*e), x))) :
    imx(v, e, m, clo, putZ(lidx(clo(*e), x)));

  y = llen(clo(*e));
  with(x, q = snoc(v, clo(*e), x));
  if (!q) return 0;
  clo(*e) = q;
  return imx(v, e, m, clo, putZ(y)); }

Co(co__) {
  ob x = *v->sp++;
  return symp(x) ? co_var(v, e, m, x) :
         twop(x) ? co_2(v, e, m, x) :
         co_x(v, e, m, x); }

Co(co_ap, ob f, ob args) {
  mm(&args);
  if (!Push(putZ(co__), f,
            putZ(i1d0), putZ(idH),
            putZ(em_call), putZ(llen(args))))
    return um, NULL;
  for (; twop(args); args = B(args))
    if (!Push(putZ(co__), A(args), putZ(i1d0), putZ(push)))
      return um, NULL;
  return um, pull(v, e, m); }

static bool seq_mo_loop(la v, ob *e, ob x) {
  if (!twop(x)) return 1;
  bool _;
  with(x, _ = seq_mo_loop(v, e, B(x)));
  return _ && Push(putZ(co__), A(x)); }

Co(co_x, ob x) {
  return Push(putZ(imm), x) ? i1d1(v, e, m) : 0; }

Co(co_q, ob x) {
  x = twop(B(x)) ? AB(x) : B(x);
  return co_x(v, e, m, x); }

Co(co_se, ob x) {
  x = B(x);
  x = twop(x) ? x : pair(v, x, nil);
  x = x ? seq_mo_loop(v, e, x) : x;
  return x ? pull(v, e, m) : 0; }

Co(co_2, ob x) {
  ob z = A(x);
  return
    z == v->lex[Quote] ? co_q(v, e, m, x) :
    z == v->lex[Cond] ? co_p(v, e, m, x) :
    z == v->lex[Lamb] ? co_t(v, e, m, x) :
    z == v->lex[Def] ? dty(v, e, m, x) :
    z == v->lex[Seq] ? co_se(v, e, m, x) :
    co_ap(v, e, m, A(x), B(x)); }

Co(i1d0) { mo k;
  vm *i = (void*) getZ(*v->sp++);
  k = pull(v, e, m+1);
  return k ? pb1(i, k): 0; }

Co(i1d1) {
  vm *i = (vm*) getZ(*v->sp++);
  ob x = *v->sp++;
  mo pf;
  with(x, pf = pull(v, e, m+2));
  return pf ? pb2(i, x, pf) : 0; }

// stack manips
static bool pushss(la v, size_t i, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return Avail >= i || please(v, i);
  bool _;
  with(x, _ = pushss(v, i+1, xs));
  return _ && (*--v->sp = x, true); }

bool pushs(la v, ...) {
  va_list xs;
  va_start(xs, v);
  bool _ = pushss(v, 0, xs);
  va_end(xs);
  return _; }

Co(co_ini) {
  ob *k = cells(v, m + 3);
  return !k ? 0 :
    (k[m+1] = 0,
     k[m+2] = (ob) k,
     k[m] = e ? name(*e) : nil,
     setw(k, nil, m),
     (mo) k + m); }
