#include "la.h"

////
///  the thread compiler
//
// this is the most complicated part of the C code but it
// normally only gets used during initialization to bootstrap the
// self-hosted compiler.
//
// " compilation environments "
typedef struct env {
  ob arg, loc, clo, name, asig, s1, s2;
  struct env *par;
} *env;
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1

static Inline mo pull(la v, env *e, size_t m) { return
  ((mo (*)(la, env*, size_t)) getnum(*v->sp++))(v, e, m); }

// apply instruction pullbacks
static Inline mo pb1(vm *i, mo k) {
  G(--k) = i;
  return k; }
static Inline mo pb2(vm *i, ob x, mo k) {
  return pb1(i, pb1((vm*) x, k)); }

#define N putnum

static bool scan(la, env*, ob) NoInline;

static mo
  r_pb1(la, env*, size_t),
  r_pb2(la, env*, size_t),
  r_co_ini(la, env*, size_t),
  r_co_x(la, env*, size_t),
  r_co_def_bind(la, env*, size_t),
  co_if(la, env*, size_t, ob) NoInline,
  co_ap(la, env*, size_t, ob, ob) NoInline,
  co_def(la, env*, size_t, ob) NoInline,
  co_fn(la, env*, size_t, ob) NoInline,
  co_seq(la, env*, size_t, ob) NoInline,
  co_sym(la, env*, size_t, ob) NoInline,
  co_two(la, env*, size_t, ob) NoInline,
  co_imm(la, env*, size_t, ob) NoInline,
  imx(la, env*, size_t, vm*, ob) NoInline;

#define Co(nom,...) static mo nom(la v, env *e, size_t m, ##__VA_ARGS__)

// supplemental list functions
//
// index of item in list (-1 if absent)
static NoInline intptr_t lidx(ob l, ob x) {
  for (intptr_t i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

// append to tail
static NoInline two snoc(la v, ob l, ob x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = (ob) snoc(v, B(l), x));
  return x ? pair(v, A(l), x) : 0; }

static NoInline ob rw_let_fn(la v, ob x) {
  mm(&x);
  for (two w; twop(A(x));)
    if (!(w = snoc(v, BA(x), AB(x)))  ||
        !(w = pair(v, v->lex[Lamb], (ob) w)) ||
        !(w = pair(v, (ob) w, BB(x))) ||
        !(x = (ob) pair(v, AA(x), (ob) w))) {
      x = 0;
      break; }
  return um, x; }

static NoInline ob asign(la v, ob a, intptr_t i, ob *m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == v->lex[Splat])
    return *m = -i-1, (ob) pair(v, A(a), nil);
  with(a, x = asign(v, B(a), i+1, m));
  return x ? (ob) pair(v, A(a), x) : 0; }

static Inline ob new_scope(la v, env *e, ob a, ob n) {
  intptr_t s = 0;
  with(n, a = asign(v, a, 0, &s));
  return !a ? 0 : Tupl(
    a, nil, nil, n, putnum(s), nil, nil, e ? (ob) *e : nil); }

static char scan_def(la v, env *e, ob x) {
  char r;
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  with(x,
     r = scan_def(v, e, BB(x)),
     r = r != 1 ? r :
       !(x = rw_let_fn(v, x)) ||
       !((*e)->loc = (ob) pair(v, A(x), (*e)->loc)) ||
       !scan(v, e, AB(x)) ? -1 : 1);
  return r; }

static bool scan(la v, env *e, ob x) {
  bool _; return
    !twop(x) || A(x) == v->lex[Lamb] || A(x) == v->lex[Quote] ? 1 :
    A(x) == v->lex[Def] ? scan_def(v, e, B(x)) != -1 :
    (with(x, _ = scan(v, e, A(x))),
     _ && scan(v, e, B(x))); }

static ob linitp(la v, ob x, ob *d) {
  ob y;
  if (!twop(B(x))) return *d = x, nil;
  with(x, y = linitp(v, B(x), d));
  return y ? (ob) pair(v, A(x), y) : 0; }

static Inline ob comp_body(la v, env *e, ob x) {
  intptr_t i;
  if (!Push(N(r_co_x), x, N(r_pb1), N(ret), N(r_co_ini)) ||
      !scan(v, e, v->sp[1]) ||
      !(x = (ob) pull(v, e, 4)))
    return 0;
  x = !(i = llen((*e)->loc)) ? x :
   (ob) pb2(setloc, putnum(i), (mo) x);
  x = (i = getnum((*e)->asig)) > 0 ?
        (ob) pb2(arity, putnum(i), (mo) x) :
      i < 0 ?
        (ob) pb2(varg, putnum(-i-1), (mo) x) :
      x;
  button((mo) x)->self = (mo) x;
  return twop((*e)->clo) ? (ob) pair(v, (*e)->clo, x) : x; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static Inline ob co_fn_ltu(la v, env *e, ob n, ob l) {
  ob y = nil;
  with(n, with(y, with(l,
    l = (l = twop(l) ? l : (ob) pair(v, l, nil)) &&
        (l = linitp(v, l, &y)) &&
        (n = (ob) pair(v, n, e ? (*e)->name : nil)) &&
        (n = new_scope(v, e, l, n)) ?
      comp_body(v, (env*) &n, A(y)) : 0)));
  return l; }

static Inline ob co_fn_clo(la v, env *e, ob arg, ob seq) {
  size_t i = llen(arg);
  mm(&arg), mm(&seq);
  if (!Push(N(r_pb2), N(take), N(i), N(r_co_ini))) arg = 0;

  for (; arg && twop(arg); arg = B(arg))
    if (!Push(N(r_co_x), A(arg), N(r_pb1), N(push))) arg = 0;

  if (arg) arg = (ob) pull(v, e, 0);
  if (arg) arg = (ob) pair(v, seq, arg);
  return um, um, arg; }

Co(co_fn, ob x) {
  vm *j = imm;
  ob k, nom = *v->sp == N(r_co_def_bind) ? v->sp[1] : nil;
  with(nom, with(x, k = (ob) pull(v, e, m+2)));
  if (!k) return 0;
  mm(&k);
  if (twop(x = co_fn_ltu(v, e, nom, B(x))))
    j = e && twop((*e)->loc) ? encl1 : encl0,
    x = co_fn_clo(v, e, A(x), B(x));
  um;
  return !x ? 0 : pb2(j, x, (mo) k); }

Co(r_co_def_bind) {
  ob _ = *v->sp++;
  if (e) return imx(v, e, m, defloc, putnum(lidx((*e)->loc, _)));
  _ = (ob) pair(v, ns_tbl(v), _);
  return _ ? imx(v, e, m, deftop, _) : 0; }

static bool co_def_r(la v, env *e, ob x) {
  bool _;
  return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = co_def_r(v, e, BB(x))), _) &&
     Push(N(r_co_x), AB(x), N(r_co_def_bind), A(x))); }

// syntactic sugar for define
static bool co_def_sugar(la v, two x) {
  ob _ = nil;
  with(_, x = (two) linitp(v, (ob) x, &_));
  return x &&
    (x = pair(v, (ob) x, _)) &&
    (x = pair(v, v->lex[Seq], (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    (x = pair(v, v->lex[Lamb], (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    Push(N(r_co_x), (ob) x); }

Co(co_def, ob x) {
  if (!twop(B(x))) return co_imm(v, e, m, nil);
  x = llen(B(x)) % 2 ? co_def_sugar(v, (two) x) : co_def_r(v, e, B(x));
  return x ? pull(v, e, m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
Co(co_if_pre) {
  ob x = (ob) pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s2) : x;
  return x ? (mo) A((*e)->s2 = x) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
Co(co_if_pre_con) {
  mo k, x = pull(v, e, m + 2);
  if (!x) return 0;
  return G(k = (mo) A((*e)->s2)) == ret ?
    pb1(ret, x) :
    pb2(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
Co(co_if_post_con) {
  ob x = (ob) pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s1) : x;
  return x ? (mo) A((*e)->s1 = x) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
Co(co_if_pre_ant) {
  mo x = pull(v, e, m+2);
  if (!x) return 0;
  x = pb2(br1, A((*e)->s1), x);
  (*e)->s1 = B((*e)->s1);
  return x; }

static bool co_if_loop(la v, env *e, ob x) {
  bool _;
  x = twop(x) ? x : (ob) pair(v, nil, nil);
  if (!x) return 0;
  if (!twop(B(x))) return
    Push(N(r_co_x), A(x), N(co_if_pre_con));
  with(x,
    _ = Push(N(co_if_post_con), N(r_co_x),
             AB(x), N(co_if_pre_con)),
    _ = _ ? co_if_loop(v, e, BB(x)) : _);
  return _ ? Push(N(r_co_x), A(x), N(co_if_pre_ant)) : 0; }

Co(co_if, ob x) {
  bool _;
  mo pf;
  with(x, _ = Push(N(co_if_pre)));
  _ = _ ? co_if_loop(v, e, x) : _;
  if (!_ || !(pf = pull(v, e, m))) return 0;
  (*e)->s2 = B((*e)->s2);
  return pf; }

Co(r_co_ap_call) {
  ob ary = *v->sp++;
  mo k = pull(v, e, m + 2);
  return k ? pb2(G(k) == ret ? rec : call, ary, k) : 0; }

enum where { Here, Loc, Arg, Clo, Wait };

static NoInline ob ls_lex(la v, env e, ob y) { return
  nilp((ob) e) ?
    (y = ns_get(v, y)) ?
      (ob) pair(v, putnum(Here), y) :
      (ob) pair(v, putnum(Wait), ns_tbl(v)) :
  lidx(e->loc, y) >= 0 ? (ob) pair(v, putnum(Loc), (ob) e) :
  lidx(e->arg, y) >= 0 ? (ob) pair(v, putnum(Arg), (ob) e) :
  lidx(e->clo, y) >= 0 ? (ob) pair(v, putnum(Clo), (ob) e) :
  ls_lex(v, (env) e->par, y); }

Co(co_sym, ob x) {
  ob y, q;
  with(x, q = ls_lex(v, e ? *e : (env) nil, x));
  if (!q) return 0;
  y = A(q);
  if (y == putnum(Here)) return co_imm(v, e, m, B(q));
  if (y == putnum(Wait)) return
    (x = (ob) pair(v, B(q), x)) &&
    (with(x, y = (ob) pull(v, e, m+2)), y) ?
      pb2(late, x, (mo) y) : 0;

  if (B(q) == (ob) *e) return
    y == putnum(Loc) ?
      imx(v, e, m, locn, putnum(lidx((*e)->loc, x))) :
    y == putnum(Arg) ?
      imx(v, e, m, argn, putnum(lidx((*e)->arg, x))) :
    imx(v, e, m, clon, putnum(lidx((*e)->clo, x)));

  y = llen((*e)->clo);
  with(x, q = (ob) snoc(v, (*e)->clo, x));
  if (!q) return 0;
  (*e)->clo = q;
  return imx(v, e, m, clon, putnum(y)); }

Co(r_co_x) {
  ob x = *v->sp++;
  return symp(x) ? co_sym(v, e, m, x) :
         twop(x) ? co_two(v, e, m, x) :
         co_imm(v, e, m, x); }

Co(co_ap, ob f, ob args) {
  mm(&args);
  if (!Push(N(r_co_x), f,
            N(r_pb1), N(idH),
            N(r_co_ap_call), N(llen(args))))
    return um, NULL;
  for (; twop(args); args = B(args))
    if (!Push(N(r_co_x), A(args), N(r_pb1), N(push)))
      return um, NULL;
  return um, pull(v, e, m); }

static bool seq_mo_loop(la v, env *e, ob x) {
  if (!twop(x)) return true;
  bool _;
  with(x, _ = seq_mo_loop(v, e, B(x)));
  return _ && Push(N(r_co_x), A(x)); }

Co(co_imm, ob x) { return
  Push(N(imm), x) ? r_pb2(v, e, m) : 0; }

Co(co_seq, ob x) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x = x ? seq_mo_loop(v, e, x) : x,
  x ? pull(v, e, m) : 0; }

Co(co_two, ob x) {
  ob a = A(x);
  if (symp(a)) {
    if (a == v->lex[Quote]) return
      x = B(x),
      co_imm(v, e, m, twop(x) ? A(x) : x);
    if (a == v->lex[Cond]) return co_if(v, e, m, B(x));
    if (a == v->lex[Lamb]) return co_fn(v, e, m, x);
    if (a == v->lex[Def]) return co_def(v, e, m, x);
    if (a == v->lex[Seq]) return co_seq(v, e, m, B(x)); }
  return co_ap(v, e, m, a, B(x)); }

Co(r_pb1) {
  vm *i = (vm*) getnum(*v->sp++);
  mo k = pull(v, e, m + 1);
  return k ? pb1(i, k): 0; }

static NoInline mo imx(la v, env *e, size_t m, vm *i, ob x) {
  mo k;
  with(x, k = pull(v, e, m+2));
  return k ? pb2(i, x, k) : 0; }

Co(r_pb2) {
  vm *i = (vm*) getnum(*v->sp++);
  ob x = *v->sp++;
  return imx(v, e, m, i, x); }

Co(r_co_ini) {
  mo k = mkmo(v, m + 1);
  if (k) setw(k, nil, m),
         G(k += m) = (vm*) (e ? (*e)->name : nil);
  return k; }

// apply expression pullbacks
static mo ana(la v, ob x) {
  bool ok = Push(N(r_co_x), x, N(r_pb1), N(ret), N(r_co_ini));
  return ok ? pull(v, 0, 0) : 0; }

// bootstrap eval interpreter function
Vm(ev_u) {
  ArityCheck(1);
  // check to see if ev has been overridden in the
  // toplevel namespace and if so call that. this way
  // ev calls compiled pre-bootstrap will use the
  // bootstrapped compiler, which is what we want?
  // seems kind of strange to need this ...
  xp = ns_get(v, v->lex[Eval]);
  if (xp && homp(xp) && G(xp) != ev_u)
    return ApY((mo) xp, nil);
  mo y;
  CallOut(y = ana(v, fp->argv[0]));
  return y ? ApY(y, xp) : ApC(xoom, xp); }
