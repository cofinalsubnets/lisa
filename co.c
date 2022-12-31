#include "i.h"
#include "vm.h"

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

typedef mo co(la, env*, size_t);

static mo
  p_pulbi(la, env*, size_t),
  p_pulbix(la, env*, size_t),
  p_mo_ini(la, env*, size_t),
  p_co_x(la, env*, size_t),
  p_co_def_bind(la, env*, size_t),
  co_x(la, env*, size_t, ob) NoInline,
  co_if(la, env*, size_t, ob) NoInline,
  co_ap(la, env*, size_t, ob, ob) NoInline,
  co_def(la, env*, size_t, ob) NoInline,
  co_fn(la, env*, size_t, ob) NoInline,
  mo_seq(la, env*, size_t, ob) NoInline,
  co_sym(la, env*, size_t, ob) NoInline,
  mo_mac(la, env*, size_t, ob, ob) NoInline,
  mo_l(la, env*, size_t, ob) NoInline,
  mo_i_x(la, env*, size_t, vm*, ob) NoInline;

mo mo_ana(la v, ob x) { return
  pushs(v, x, p_pulbi, ret, p_mo_ini, NULL) ?
    p_co_x(v, 0, 0) :
    0; }

static Inline mo co_pull(la v, env *e, size_t m) { return
  ((mo (*)(la, env*, U)) (*v->sp++))(v, e, m); }

static mo seq(la v, mo a, mo b) {
  return thd(v, imm, a, call, nil, imm, b, rec, nil, NULL); }

#define Co(nom,...)\
  static mo nom(la v, env *e, size_t m, ##__VA_ARGS__)

static bool scan(la, env*, ob) NoInline;

// apply instruction pullbacks
static Inline mo pulbi(vm *i, mo k) { return k--, G(k) = i, k; }
static Inline mo pulbix(vm *i, ob x, mo k) {
  return pulbi(i, pulbi((vm*) x, k)); }

// supplemental list functions
//

// index of item in list (-1 if absent)
static NoInline I lidx(ob l, ob x) {
  for (I i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

// append to tail
static NoInline two snoc(la v, ob l, ob x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = (ob) snoc(v, B(l), x));
  return x ? pair(v, A(l), x) : 0; }

static NoInline ob rw_let_fn(la v, ob x) {
  mm(&x);
  for (two w; x && twop(A(x)); x =
    (w = snoc(v, BA(x), AB(x))) &&
    (w = pair(v, (ob) v->lex.lambda, (ob) w)) &&
    (w = pair(v, (ob) w, BB(x))) ?
      (ob) pair(v, AA(x), (ob) w) : 0);
  return um, x; }

static NoInline ob asign(la v, ob a, intptr_t i, ob *m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == (ob) v->lex.splat)
    return *m = -i - 1, (ob) pair(v, A(a), nil);
  return with(a, x = asign(v, B(a), i + 1, m)),
    x ? (ob) pair(v, A(a), x) : 0; }

static Inline ob new_scope(la v, env *e, ob arg, ob nom) {
  env f;
  I asig = 0;
  with(nom,
    arg = asign(v, arg, 0, &asig),
    with(arg, f = (env) mo_n(v, Width(struct env))));
  if (f)
    f->arg = arg,
    f->name = nom,
    f->asig = putnum(asig),
    f->loc = f->clo = f->s1 = f->s2 = nil,
    f->par = e ? *e : (env) nil;
  return (ob) f; }

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
  bool _; return !twop(x) ||
    A(x) == (ob) v->lex.lambda ||
    A(x) == (ob) v->lex.quote ||
    (A(x) == (ob) v->lex.define &&
     scan_def(v, e, B(x)) != -1) ||
    (with(x, _ = scan(v, e, A(x))),
     _ && scan(v, e, B(x))); }

static NoInline ob linitp(la v, ob x, ob *d) {
  if (!twop(B(x))) return *d = x, nil;
  ob y; return
    with(x, y = linitp(v, B(x), d)),
    y ? (ob) pair(v, A(x), y) : 0; }

static Inline ob comp_body(la v, env *e, ob x) {
  I i;
  if (!pushs(v, p_co_x, x, p_pulbi, ret, p_mo_ini, NULL) ||
      !scan(v, e, v->sp[1]) ||
      !(x = (ob) co_pull(v, e, 4)))
    return 0;
  x = !(i = llen((*e)->loc)) ? x :
   (ob) pulbix(setloc, putnum(i), (mo) x);
  x = (i = getnum((*e)->asig)) > 0 ?
        (ob) pulbix(arity, putnum(i), (mo) x) :
      i < 0 ?
        (ob) pulbix(varg, putnum(-i-1), (mo) x) :
      x;
  return mo_tag((mo) x)->head = (mo) x,
    !twop((*e)->clo) ? x : (ob) pair(v, (*e)->clo, x); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static ob co_fn_ltu(la v, env *e, ob n, ob l) {
  ob y = nil; return
    with(n, with(y, with(l,
      l = (l = twop(l) ? l : (ob) pair(v, l, nil)) &&
          (l = linitp(v, l, &y)) &&
          (n = (ob) pair(v, n, e ? (*e)->name : nil)) &&
          (n = new_scope(v, e, l, n)) ?
        comp_body(v, (env*) &n, A(y)) : 0))),
    l; }

static ob co_fn_clo(la v, env *e, ob vars, ob code) {
  U i = llen(vars);
  mm(&vars), mm(&code);

  vars = pushs(v, p_pulbix, take, putnum(i), p_mo_ini, NULL) ? vars : 0;
  while (vars && i--) vars =
    pushs(v, p_co_x, A(vars), p_pulbi, push, NULL) ? B(vars) : 0;
  vars = vars ? (ob) co_pull(v, e, 0) : vars;
  vars = vars ? (ob) pair(v, code, vars) : vars;
  return um, um, vars; }

Co(co_fn_enclose, ob x, mo k) { return
  with(k, x = co_fn_clo(v, e, A(x), B(x))),
  // FIXME no locals => no need to defer closure construction
  x ? pulbix(e && homp((*e)->loc) ? encl1 : encl0, x, k) : 0; }

Co(co_fn, ob x) {
  ob nom = *v->sp == (ob) p_co_def_bind ? v->sp[1] : nil;
  mo k; with(nom, with(x, k = co_pull(v, e, m+2)));
  if (!k) return 0;
  with(k, x = co_fn_ltu(v, e, nom, x));
  return !x ? 0 : G(x) == act ? co_fn_enclose(v, e, m, x, k) : pulbix(imm, x, k); }

Co(p_co_def_bind) {
  ob _ = *v->sp++;
  if (!e) return
    _ = (ob) pair(v, (ob) v->topl, _),
    _ ? mo_i_x(v, e, m, deftop, _) : 0;
  return mo_i_x(v, e, m, defsl1, putnum(lidx((*e)->loc, _))); }

static bool co_def_r(la v, env *e, ob x) {
  bool _; return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = co_def_r(v, e, BB(x))), _) &&
     pushs(v, p_co_x, AB(x), p_co_def_bind, A(x), NULL)); }

// syntactic sugar for define
static bool co_def_sug(la v, two x) {
  ob _ = nil; return
    (with(_, x = (two) linitp(v, (ob) x, &_)), x) &&
    (x = pair(v, (ob) x, _)) &&
    (x = pair(v, (ob) v->lex.begin, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    (x = pair(v, (ob) v->lex.lambda, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    pushs(v, p_co_x, x, NULL); }

Co(co_def, ob x) { return
  !twop(B(x)) ? mo_i_x(v, e, m, imm, nil) :
  (llen(B(x)) & 1 ?
   co_def_sug(v, (two) x) : co_def_r(v, e, B(x))) ?
    co_pull(v, e, m) :
    0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
Co(co_if_pre) {
  ob x = (ob) co_pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s2) : x;
  return x ? (mo) A((*e)->s2 = x) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
Co(co_if_pre_con) {
  mo k, x = co_pull(v, e, m + 2);
  return !x ? 0 : G(k = (mo) A((*e)->s2)) == ret ?
    pulbi(ret, x) : pulbix(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
Co(co_if_post_con) {
  ob x = (ob) co_pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s1) : x;
  return x ? (mo) A((*e)->s1 = x) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
Co(co_if_pre_ant) {
  mo x = co_pull(v, e, m + 2);
  if (!x) return 0;
  x = pulbix(br1, A((*e)->s1), x);
  (*e)->s1 = B((*e)->s1);
  return x; }

static bool co_if_loop(la v, env *e, ob x) {
  bool _;
  x = twop(x) ? x : (ob) pair(v, nil, nil);
  if (!x) return false;
  if (!twop(B(x))) return
    pushs(v, p_co_x, A(x), co_if_pre_con, NULL);
  with(x,
    _ = pushs(v, co_if_post_con, p_co_x,
              AB(x), co_if_pre_con, NULL),
    _ = _ ? co_if_loop(v, e, BB(x)) : _);
  return _ ? pushs(v, p_co_x, A(x), co_if_pre_ant, NULL) : 0; }

Co(co_if, ob x) {
  bool _;
  with(x, _ = pushs(v, co_if_pre, NULL));
  _ = _ && co_if_loop(v, e, x);
  if (!_) return 0;
  mo pf = co_pull(v, e, m);
  if (pf) (*e)->s2 = B((*e)->s2);
  return pf; }

Co(p_co_ap_call) {
  ob ary = *v->sp++;
  mo k = co_pull(v, e, m + 2);
  return k ? pulbix(G(k) == ret ? rec : call, ary, k) : 0; }

enum where { Arg, Loc, Clo, Here, Wait };

static NoInline ob co_sym_look(la v, env e, ob y) { return
  nilp((ob) e) ?
    (y = tbl_get(v, v->topl, y, 0)) ?
      (ob) pair(v, Here, y) :
      (ob) pair(v, Wait, (ob) v->topl) :
  lidx(e->loc, y) >= 0 ? (ob) pair(v, Loc, (ob) e) :
  lidx(e->arg, y) >= 0 ? (ob) pair(v, Arg, (ob) e) :
  lidx(e->clo, y) >= 0 ? (ob) pair(v, Clo, (ob) e) :
  co_sym_look(v, (env) e->par, y); }

Co(co_sym, ob x) {
  ob q;
  with(x, q = co_sym_look(v, e ? *e : (env) nil, x));
  if (!q) return 0;
  if (A(q) == Here) return mo_i_x(v, e, m, imm, B(q));
  if (A(q) == Wait) return
    (x = (ob) pair(v, B(q), x)) &&
    (with(x, q = (ob) co_pull(v, e, m + 2)), q) ?
      pulbix(late, x, (mo) q) : 0;

  if (B(q) == (ob) *e) {
    ob idx = putnum(lidx(((ob*)(*e))[A(q)], x));
    vm *i = A(q) == Arg ? argn : A(q) == Loc ? sl1n : clon;
    return mo_i_x(v, e, m, i, idx); }

  U y = llen((*e)->clo);
  if (!(q = (ob) snoc(v, (*e)->clo, x))) return 0;
  return (*e)->clo = q, mo_i_x(v, e, m, clon, putnum(y)); }

Co(co_x, ob x) { return
  symp(x) ? co_sym(v, e, m, x) :
  twop(x) ? mo_l(v, e, m, x) :
  mo_i_x(v, e, m, imm, x); }

Co(p_co_x) { return co_x(v, e, m, *v->sp++); }

Co(co_ap, ob f, ob args) {
  mm(&args);
  if (!pushs(v,
        p_co_x, f,
        p_pulbi, idmo,
        p_co_ap_call, putnum(llen(args)),
        NULL))
    return um, NULL;
  for (; twop(args); args = B(args))
    if (!pushs(v, p_co_x, A(args), p_pulbi, push, NULL))
      return um, NULL;
  return um, co_pull(v, e, m); }

static NoInline bool seq_mo_loop(la v, env *e, ob x) {
  bool _; return !twop(x) ||
    (with(x, _ = seq_mo_loop(v, e, B(x))),
     _ && pushs(v, p_co_x, A(x), NULL)); }

Co(mo_seq, ob x) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x && seq_mo_loop(v, e, x) ?
    co_pull(v, e, m) :
    0; }

static NoInline enum status la_call(la v, mo f, size_t n) {
  return v->ip = thd(v, imm, f, call, putnum(n), xok, NULL),
         !v->ip ? OomError : la_go(v); }

NoInline enum status la_ev_x(la v, ob x) {
  mo k = thd(v,
    imm, x, push,
    imm, ev_f,
    rec, putnum(1),
    ev_f, NULL);
  if (!k) return OomError;
  return
    k[4].ap = (vm*) (k + 7),
    la_call(v, k, 0); }

static enum status la_ap(la v, mo f, ob x) {
  mo k = thd(v,
    imm, x, push,
    imm, f, push,
    imm, ap_f,
    rec, putnum(2),
    ap_f, NULL);
  if (!k) return OomError;
  return
    k[7].ap = (vm*) (k + 10),
    la_call(v, k, 0); }

Co(mo_mac, ob mac, ob x) {
  enum status s;
  ob xp = v->xp;
  mo ip = v->ip;
  return
    with(xp, with(ip, s = la_ap(v, (mo) mac, x))),
    x = v->xp, v->xp = xp, v->ip = ip,
    report(v, s),
    s == Ok ? co_x(v, e, m, x) : NULL; }

Co(mo_l, ob x) {
  ob a = A(x);
  if (symp(a)) {
    sym y = (sym) a;
    if (y == v->lex.quote) return
      mo_i_x(v, e, m, imm, twop(B(x)) ? AB(x) : B(x));
    if (y == v->lex.cond)   return co_if(v, e, m, B(x));
    if (y == v->lex.lambda) return co_fn(v, e, m, B(x));
    if (y == v->lex.define) return co_def(v, e, m, x);
    if (y == v->lex.begin)  return mo_seq(v, e, m, B(x)); }
  if ((a = tbl_get(v, v->macros, a, 0)))
    return mo_mac(v, e, m, a, B(x));
  return co_ap(v, e, m, A(x), B(x)); }

Co(p_pulbi) {
  vm *i = (vm*) *v->sp++;
  mo k = co_pull(v, e, m + 1);
  return k ? pulbi(i, k): 0; }

static mo mo_i_x(la v, env *e, size_t m, vm *i, ob x) {
  mo k; return
    with(x, k = co_pull(v, e, m + 2)),
    k ? pulbix(i, x, k) : 0; }

Co(p_pulbix) {
  vm *i = (vm*) *v->sp++;
  ob x = *v->sp++;
  return mo_i_x(v, e, m, i, x); }

Co(p_mo_ini) {
  mo k = mo_n(v, m + 1);
  if (k) setw(k, nil, m),
         G(k += m) = (vm*) (e ? (*e)->name : nil);
  return k; }
