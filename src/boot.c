#include "la.h"
#include "boot.h"
#include "two.h"
#include "mo.h"
#include "ns.h"
#include "alloc.h"
#include "vm.h"
#include "gc.h"
#include <string.h>

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
  struct env *par; } *env;
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1

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
  imx(la, env*, size_t, vm*, ob) NoInline;

static Inline mo pull(la v, env *e, size_t m) { return
  ((mo (*)(la, env*, size_t)) (*v->sp++))(v, e, m); }

mo ana(la v, ob x) {
  bool ok = pushs(v, r_co_x, x, r_pb1, ret, r_co_ini, NULL);
  return ok ? pull(v, 0, 0) : 0; }

#define Co(nom,...) static mo nom(la v, env *e, size_t m, ##__VA_ARGS__)

static bool scan(la, env*, ob) NoInline;

// apply instruction pullbacks
static Inline mo pb1(vm *i, mo k) {
  return G(--k) = i, k; }
static Inline mo pb2(vm *i, ob x, mo k) {
  return pb1(i, pb1((vm*) x, k)); }


// supplemental list functions
//
// index of item in list (-1 if absent)
static intptr_t lidx(ob l, ob x) {
  for (intptr_t i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

// length of list
static size_t llen(ob l) {
  size_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

// append to tail
static NoInline two snoc(la v, ob l, ob x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = (ob) snoc(v, B(l), x));
  return x ? pair(v, A(l), x) : 0; }

static NoInline ob rw_let_fn(la v, ob x) {
  mm(&x);
  for (two w; x && twop(A(x)); x =
    (w = snoc(v, BA(x), AB(x))) &&
    (w = pair(v, (ob) v->lex[Lamb], (ob) w)) &&
    (w = pair(v, (ob) w, BB(x))) ?
      (ob) pair(v, AA(x), (ob) w) : 0);
  return um, x; }

static NoInline ob asign(la v, ob a, intptr_t i, ob *m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == (ob) v->lex[Splat])
    return *m = -i - 1, (ob) pair(v, A(a), nil);
  with(a, x = asign(v, B(a), i + 1, m));
  return x ? (ob) pair(v, A(a), x) : 0; }

static Inline ob new_scope(la v, env *e, ob arg, ob nom) {
  intptr_t asig = 0;
  with(nom, arg = asign(v, arg, 0, &asig));
  return !arg ? 0 : tupl(v,
    arg, // positional arguments
    nil, // local variables
    nil, // closure variables
    nom, // function name
    putnum(asig), // arity signature
    nil, // address stack 1
    nil, // address stack 2
    e ? (ob) *e : nil, // parent scope
    NULL); }

static NoInline int scan_def(la v, env *e, ob x) {
  int r;
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  with(x,
     r = scan_def(v, e, BB(x)),
     r = r != 1 ? r :
       !(x = rw_let_fn(v, x)) ||
       !((*e)->loc = (ob) pair(v, A(x), (*e)->loc)) ||
       !scan(v, e, AB(x)) ? -1 : 1);
  return r; }

static NoInline bool scan(la v, env *e, ob x) {
  if (!twop(x) ||
      A(x) == (ob) v->lex[Lamb] ||
      A(x) == (ob) v->lex[Quote])
    return true;
  if (A(x) == (ob) v->lex[Def])
    return scan_def(v, e, B(x)) != -1;
  bool _;
  with(x, _ = scan(v, e, A(x)));
  return _ && scan(v, e, B(x)); }

static NoInline ob linitp(la v, ob x, ob *d) {
  ob y;
  if (!twop(B(x))) return *d = x, nil;
  with(x, y = linitp(v, B(x), d));
  return y ? (ob) pair(v, A(x), y) : 0; }

static Inline ob comp_body(la v, env *e, ob x) {
  intptr_t i;
  if (!pushs(v, r_co_x, x, r_pb1, ret, r_co_ini, NULL) ||
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
  motag((mo) x)->head = (mo) x;
  return !twop((*e)->clo) ? x :
    (ob) pair(v, (*e)->clo, x); }

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

  arg = pushs(v, r_pb2, take, putnum(i), r_co_ini, NULL) ?
    arg : 0;

  while (arg && twop(arg)) arg =
    pushs(v, r_co_x, A(arg), r_pb1, push, NULL) ?
      B(arg) : 0;

  arg = arg ? (ob) pull(v, e, 0) : arg;
  arg = arg ? (ob) pair(v, seq, arg) : arg;
  return um, um, arg; }

Co(co_fn, ob x) {
  vm *j = imm;
  ob k, nom = *v->sp == (ob) r_co_def_bind ? v->sp[1] : nil;
  with(nom, with(x, k = (ob) pull(v, e, m+2)));
  if (!k) return 0;
  mm(&k);
  x = co_fn_ltu(v, e, nom, B(x));
  if (x && twop(x))
    j = e && twop((*e)->loc) ? encl1 : encl0,
    x = co_fn_clo(v, e, A(x), B(x));
  um;
  return x ? pb2(j, x, (mo) k) : 0; }

Co(r_co_def_bind) {
  ob _ = *v->sp++;
  if (e) return imx(v, e, m, defloc, putnum(lidx((*e)->loc, _)));
  _ = (ob) pair(v, nstbl(v), _);
  return _ ? imx(v, e, m, deftop, _) : 0; }

static bool co_def_r(la v, env *e, ob x) {
  if (!twop(x)) return true;
  bool _;
  return (x = rw_let_fn(v, x)) &&
   (with(x, _ = co_def_r(v, e, BB(x))), _) &&
   pushs(v, r_co_x, AB(x), r_co_def_bind, A(x), NULL); }

// syntactic sugar for define
static Inline bool co_def_sugar(la v, two x) {
  ob _ = nil;
  with(_, x = (two) linitp(v, (ob) x, &_));
  return x &&
    (x = pair(v, (ob) x, _)) &&
    (x = pair(v, (ob) v->lex[Seq], (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    (x = pair(v, (ob) v->lex[Lamb], (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    pushs(v, r_co_x, x, NULL); }

Co(co_def, ob x) {
  if (!twop(B(x))) return imx(v, e, m, imm, nil);
  x = llen(B(x)) % 2 ?
    co_def_sugar(v, (two) x) :
    co_def_r(v, e, B(x));
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
  if (!x) return false;
  if (!twop(B(x))) return
    pushs(v, r_co_x, A(x), co_if_pre_con, NULL);
  with(x,
    _ = pushs(v, co_if_post_con, r_co_x,
             AB(x), co_if_pre_con, NULL),
    _ = _ ? co_if_loop(v, e, BB(x)) : _);
  return _ ? pushs(v, r_co_x, A(x), co_if_pre_ant, NULL) : 0; }

Co(co_if, ob x) {
  bool _;
  with(x, _ = pushs(v, co_if_pre, NULL));
  _ = _ && co_if_loop(v, e, x);
  if (!_) return 0;
  mo pf = pull(v, e, m);
  if (pf) (*e)->s2 = B((*e)->s2);
  return pf; }

Co(r_co_ap_call) {
  ob ary = *v->sp++;
  mo k = pull(v, e, m + 2);
  return k ? pb2(G(k) == ret ? rec : call, ary, k) : 0; }

enum where { Here, Loc, Arg, Clo, Wait };

static NoInline ob ls_lex(la v, env e, ob y) { return
  nilp((ob) e) ?
    (y = nsget(v, y)) ?
      (ob) pair(v, Here, y) :
      (ob) pair(v, Wait, nstbl(v)) :
  lidx(e->loc, y) >= 0 ? (ob) pair(v, Loc, (ob) e) :
  lidx(e->arg, y) >= 0 ? (ob) pair(v, Arg, (ob) e) :
  lidx(e->clo, y) >= 0 ? (ob) pair(v, Clo, (ob) e) :
  ls_lex(v, (env) e->par, y); }

Co(co_sym, ob x) {
  ob y, q;
  with(x, q = ls_lex(v, e ? *e : (env) nil, x));
  if (!q) return 0;
  y = A(q);
  if (y == Here) return imx(v, e, m, imm, B(q));
  if (y == Wait) return
    (x = (ob) pair(v, B(q), x)) &&
    (with(x, y = (ob) pull(v, e, m+2)), y) ?
      pb2(late, x, (mo) y) : 0;

  if (B(q) == (ob) *e) return
    y == Loc ?
      imx(v, e, m, locn, putnum(lidx((*e)->loc, x))) :
    y == Arg ?
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
         imx(v, e, m, imm, x); }

Co(co_ap, ob f, ob args) {
  mm(&args);
  if (!pushs(v,
        r_co_x, f,
        r_pb1, idmo,
        r_co_ap_call, putnum(llen(args)),
        NULL))
    return um, NULL;
  for (; twop(args); args = B(args))
    if (!pushs(v, r_co_x, A(args), r_pb1, push, NULL))
      return um, NULL;
  return um, pull(v, e, m); }

static NoInline bool seq_mo_loop(la v, env *e, ob x) {
  if (!twop(x)) return true;
  bool _;
  with(x, _ = seq_mo_loop(v, e, B(x)));
  return _ && pushs(v, r_co_x, A(x), NULL); }

Co(co_seq, ob x) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x = x && seq_mo_loop(v, e, x),
  x ? pull(v, e, m) : 0; }

Co(co_two, ob x) {
  ob a = A(x);
  if (symp(a)) {
    sym y = (sym) a;
    if (y == v->lex[Quote]) return
      imx(v, e, m, imm, twop(B(x)) ? AB(x) : B(x));
    if (y == v->lex[Cond]) return co_if(v, e, m, B(x));
    if (y == v->lex[Lamb]) return co_fn(v, e, m, x);
    if (y == v->lex[Def]) return co_def(v, e, m, x);
    if (y == v->lex[Seq]) return co_seq(v, e, m, B(x)); }
  return co_ap(v, e, m, a, B(x)); }

Co(r_pb1) {
  vm *i = (vm*) *v->sp++;
  mo k = pull(v, e, m + 1);
  return k ? pb1(i, k): 0; }

static NoInline mo imx(la v, env *e, size_t m, vm *i, ob x) {
  mo k;
  with(x, k = pull(v, e, m + 2));
  return k ? pb2(i, x, k) : 0; }

Co(r_pb2) {
  vm *i = (vm*) (*v->sp++);
  ob x = *v->sp++;
  return imx(v, e, m, i, x); }

Co(r_co_ini) {
  mo k = mkmo(v, m + 1);
  if (k) memset(k, -1, m * sizeof(ob)),
         G(k += m) = (vm*) (e ? (*e)->name : nil);
  return k; }
