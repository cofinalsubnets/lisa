#include "i.h"

NoInline enum status li_go(li v) {
  mo ip; frame fp; ob xp, *hp, *sp;
  return Unpack(), ApY(ip, xp); }

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

static NoInline ob asign(la v, ob a, intptr_t i, ob *m) {
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == (ob) v->lex->splat)
    return *m = -i - 1, (ob) pair(v, A(a), nil);
  ob x; with(a, x = asign(v, B(a), i + 1, m));
  return !x ? 0 : (ob) pair(v, A(a), x); }

static NoInline ob new_scope(li v, env *e, ob arg, ob nom) {
  intptr_t asig = 0;
  with(nom, arg = asign(v, arg, 0, &asig));
  return !arg ? 0 : (ob) thd(v,
    arg, nil, nil, // arg loc clo
    nom, putnum(asig), // name asig
    nil, nil, // s1 s2
    e ? *e : (env) nil, // par
    EndArgs); }

typedef mo co(li, env*, size_t);

static mo
  em1(li, env*, size_t),
  em2(li, env*, size_t),
  ana_alloc(li, env*, size_t),
  p_co_x(li, env*, size_t),
  ana_define(li, env*, size_t, ob) NoInline,
  ana_lambda(li, env*, size_t, ob) NoInline,
  ana_macro(li, env*, size_t, ob, ob) NoInline,
  ana_list(li, env*, size_t, ob) NoInline,
  ana_i(li, env*, size_t, vm*) NoInline,
  ana_i_x(li, env*, size_t, vm*, ob) NoInline;

mo ana(li v, ob x) { return
  !pushs(v, x, em1, ret, ana_alloc, EndArgs) ? NULL :
    p_co_x(v, NULL, 0); }

static NoInline mo co_pull(li v, env *e, size_t m) { return
  ((mo (*)(li, env*, size_t)) (*v->sp++))(v, e, m); }

// apply instruction pullbacks
static Inline mo pulli(vm *i, mo k) {
  return k--, G(k) = i, k; }

static NoInline mo pullix(vm *i, ob x, mo k) {
  k -= 2;
  G(k) = i, GF(k) = (vm*) x;
  return k; }

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
  for (two w; x && twop(A(x)); x =
    (w = snoc(v, BA(x), AB(x))) &&
    (w = pair(v, (ob) v->lex->lambda, (ob) w)) &&
    (w = pair(v, (ob) w, BB(x))) ?
      (ob) pair(v, AA(x), (ob) w) : 0);
  return um, x; }

static bool scan(li, env*, ob) NoInline;
static NoInline int scan_def(li v, env *e, ob x) {
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

static NoInline bool scan(li v, env *e, ob x) {
  bool _; return !twop(x) ||
    A(x) == (ob) v->lex->lambda ||
    A(x) == (ob) v->lex->quote ||
    (A(x) == (ob) v->lex->define &&
     scan_def(v, e, B(x)) != -1) ||
    (with(x, _ = scan(v, e, A(x))),
     _ && scan(v, e, B(x))); }

static NoInline ob linitp(la v, ob x, ob *d) {
  if (!twop(B(x))) return *d = x, nil;
  ob y; with(x, y = linitp(v, B(x), d));
  return y ? (ob) pair(v, A(x), y) : 0; }

#define Co(nom,...)\
  mo nom(la v, env *e, size_t m, ##__VA_ARGS__)

static Co(p_ana_define_bind) {
  ob _ = *v->sp++;
  if (!e) return
    _ = (ob) pair(v, (ob) v->lex->topl, _),
    _ ? ana_i_x(v, e, m, deftop, _) : 0;
  return ana_i_x(v, e, m, defsl1, putnum(lidx((*e)->loc, _))); }

static Inline ob ana_lambda_body(la v, env *e, ob x) {
  intptr_t i;
  if (!pushs(v, p_co_x, x, em1, ret, ana_alloc, EndArgs) ||
      !scan(v, e, v->sp[1]) ||
      !(x = (ob) co_pull(v, e, 4)))
    return 0;

  x = !(i = llen((*e)->loc)) ? x :
   (ob) pullix(setloc, putnum(i), (mo) x);
  x = (i = getnum((*e)->asig)) > 0 ?
        (ob) pullix(arity, putnum(i), (mo) x) :
      i < 0 ?
        (ob) pullix(varg, putnum(-i-1), (mo) x) :
      x;

  mo_tag((mo) x)->head = (mo) x;
  return !twop((*e)->clo) ? x :
    (ob) pair(v, (*e)->clo, x); }

static NoInline bool ana_lambda_b_enclose(li v, env *e, ob x) {
  ob vars = A(x), code = B(x);
  size_t i = llen(vars);
  mm(&vars); mm(&code);
  vars = pushs(v, em2, take, putnum(i), ana_alloc, EndArgs) ? vars : 0;
  while (vars && i--) vars =
    pushs(v, p_co_x, A(vars), em1, push, EndArgs) ? B(vars) : 0;
  vars = vars ? (ob) co_pull(v, e, 0) : vars;
  vars = vars ? (ob) pair(v, code, vars) : vars;
  um, um;
  if (!vars) return false;
  vm *j = e && homp((*e)->loc) ? encl1 : encl0;
  return pushs(v, em2, j, vars, EndArgs); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).

static NoInline bool ana_lambda_b(li v, env *e, ob x) {
  ob y = nil,
     n = *v->sp == (ob) p_ana_define_bind ? v->sp[1] : nil;
  mm(&n); mm(&x); mm(&y);
  x = (x = twop(x) ? x : (ob) pair(v, x, nil)) &&
      (x = linitp(v, x, &y)) &&
      (n = (ob) pair(v, n, e ? (*e)->name : nil)) &&
      (n = new_scope(v, e, x, n)) ?
        ana_lambda_body(v, (env*) &n, A(y)) :
        0;
  um, um, um;
  return !x ? 0 :
    G(x) == act ? ana_lambda_b_enclose(v, e, x) :
    pushs(v, em2, imm, x, EndArgs); }

static Co(ana_lambda, ob x) { return
  ana_lambda_b(v, e, x) ? co_pull(v, e, m) : 0; }

static NoInline bool ana_define_r(la v, env *e, ob x) {
  bool _;
  return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = ana_define_r(v, e, BB(x))), _) &&
     pushs(v, p_co_x, AB(x), p_ana_define_bind, A(x), EndArgs)); }

// syntactic sugar for define
static NoInline bool ana_define_sug(la v, two x) {
  ob _ = nil; return
    (with(_, x = (two) linitp(v, (ob) x, &_)), x) &&
    (x = pair(v, (ob) x, _)) &&
    (x = pair(v, (ob) v->lex->begin, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    (x = pair(v, (ob) v->lex->lambda, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    pushs(v, p_co_x, x, EndArgs); }

static NoInline bool ana_define_b(li v, env *e, ob x) {
  ob b = B(x);
  if (!twop(b)) return pushs(v, em1, imm0, EndArgs);
  return llen(b) & 1 ?
    ana_define_sug(v, (two) x) :
    ana_define_r(v, e, b); }

static NoInline Co(ana_define, ob x) { return
  ana_define_b(v, e, x) ? co_pull(v, e, m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
static NoInline Co(ana_cond_pre) {
  ob x = (ob) co_pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s2) : x;
  return !x ? 0 : (mo) A((*e)->s2 = x); }

// before generating a branch emit a jump to
// the top of stack 2
static Co(ana_cond_pre_con) {
  mo x = co_pull(v, e, m + 2);
  if (!x) return x;
  mo k = (mo) A((*e)->s2);
  return G(k) == ret ? pulli(ret, x) :
                       pullix(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
static Co(ana_cond_post_con) {
  ob x = (ob) co_pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s1) : x;
  return x ? (mo) A((*e)->s1 = x) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
static Co(ana_cond_pre_ant) {
  mo x = co_pull(v, e, m + 2);
  if (!x) return 0;
  x = pullix(br1, A((*e)->s1), x);
  (*e)->s1 = B((*e)->s1);
  return x; }

static NoInline bool ana_cond_loop(la v, env *e, ob x) {
  x = twop(x) ? x : (ob) pair(v, nil, nil);
  if (!x) return false;
  if (!twop(B(x))) return
    pushs(v, p_co_x, A(x), ana_cond_pre_con, EndArgs);
  bool _; with(x,
    _ = pushs(v, ana_cond_post_con, p_co_x,
              AB(x), ana_cond_pre_con, EndArgs),
    _ = _ ? ana_cond_loop(v, e, BB(x)) : _);
  return _ ? pushs(v, p_co_x, A(x), ana_cond_pre_ant, EndArgs) : 0; }

static NoInline Co(ana_cond_post) {
  mo k = co_pull(v, e, m);
  (*e)->s2 = B((*e)->s2);
  return k; }
static NoInline Co(ana_cond, ob x) {
  bool _;
  with(x, _ = pushs(v, ana_cond_pre, EndArgs));
  _ = _ && ana_cond_loop(v, e, x) && pushs(v, ana_cond_post, EndArgs);
  return !_ ? 0 : co_pull(v, e, m); }

static NoInline Co(p_ana_ap_call) {
  ob ary = *v->sp++;
  mo k = co_pull(v, e, m + 2);
  return !k ? k :
    G(k) == ret ? pullix(rec, ary, k + 1) :
    pullix(call, ary, k); }

enum where { Arg, Loc, Clo, Here, Wait };
static NoInline enum where ana_sym_look(la v, env e, ob y, ob *p) { return
  nilp((ob) e) ?
    (y = tbl_get(v, v->lex->topl, y, 0)) ?
      (*p = y, Here) :
      (*p = (ob) v->lex->topl, Wait) :
  lidx(e->loc, y) >= 0 ? (*p = (ob) e, Loc) :
  lidx(e->arg, y) >= 0 ? (*p = (ob) e, Arg) :
  lidx(e->clo, y) >= 0 ? (*p = (ob) e, Clo) :
  ana_sym_look(v, (env) e->par, y, p); }

static NoInline Co(ana_sym_lazy, ob x, ob b) { return
  (x = (ob) pair(v, b, x)) &&
  (with(x, b = (ob) co_pull(v, e, m + 2)), b) ?
  pullix(late, x, (mo) b) : 0; }

static NoInline Co(ana_sym_ref, ob x, ob a) {
  ob idx = putnum(lidx(((ob*)(*e))[a], x));
  vm *i = a == Arg ? argn : a == Clo ? clon : sl1n;
  return ana_i_x(v, e, m, i, idx); }

static NoInline Co(ana_sym, ob x) {
  ob b;
  enum where a = ana_sym_look(v, e ? *e : (env) nil, x, &b);
  if (a == Here) return ana_i_x(v, e, m, imm, b);
  if (a == Wait) return ana_sym_lazy(v, e, m, x, b); 
  if (b == (ob) *e) return ana_sym_ref(v, e, m, x, a);
  size_t y = llen((*e)->clo);
  if (!(x = (ob) snoc(v, (*e)->clo, x))) return 0;
  return (*e)->clo = x, ana_i_x(v, e, m, clon, putnum(y)); }

static NoInline Co(co_x, ob x) { return
  symp(x) ? ana_sym(v, e, m, x) :
  twop(x) ? ana_list(v, e, m, x) :
  ana_i_x(v, e, m, imm, x); }

static Co(p_co_x) { return co_x(v, e, m, *v->sp++); }

static NoInline bool ana_ap_b(li v, ob f, ob x) {
  mm(&x);
  bool ok = pushs(v,
    p_co_x, f,
    em1, idmo,
    p_ana_ap_call, putnum(llen(x)),
    EndArgs);
  for (; ok && twop(x); x = B(x)) ok =
    pushs(v, p_co_x, A(x), em1, push, EndArgs);
  return um, ok; }

static NoInline Co(ana_ap, ob f, ob args) {
  return ana_ap_b(v, f, args) ? co_pull(v, e, m) : 0; }

static NoInline bool ana_begin_loop(la v, env *e, ob x) {
  bool _; return !twop(x) ||
    (with(x, _ = ana_begin_loop(v, e, B(x))),
     _ && pushs(v, p_co_x, A(x), EndArgs)); }

static NoInline bool ana_begin_b(li v, env *e, ob x) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x && ana_begin_loop(v, e, x); }

static NoInline Co(ana_begin, ob x) { return
  ana_begin_b(v, e, x) ? co_pull(v, e, m) : 0; }

static enum status li_ap(la v, mo f, ob x) {
  mo k = thd(v,
    immp, x,
    immp, f,
    imm, nil, // assignment target idx=5
    call, putnum(2),
    xok, ap_f, // source idx=9
    EndArgs);
  if (!k) return OomError;
  return k[5].ap = (vm*) (k + 9),
         v->ip = k,
         li_go(v); }

static NoInline bool ana_macro_b(li v, ob mac, ob x) {
  enum status s;
  ob xp = v->xp;
  mo ip = v->ip;
  with(xp, with(ip, s = li_ap(v, (mo) mac, x)));
  x = v->xp, v->xp = xp, v->ip = ip;
  if (s != Ok) return report(v, s), 0;
  return pushs(v, p_co_x, x, EndArgs); }

static NoInline Co(ana_macro, ob mac, ob x) {
  return ana_macro_b(v, mac, x) ? co_pull(v, e, m) : 0; }

static NoInline Co(ana_list, ob x) {
  ob a = A(x), b = B(x);
  if (symp(a)) {
    sym y = (sym) a;
    if (y == v->lex->quote) return
      ana_i_x(v, e, m, imm, twop(b) ? A(b) : b); // b (em2)
    if (y == v->lex->cond)   return ana_cond(v, e, m, b); // b
    if (y == v->lex->lambda) return ana_lambda(v, e, m, b); // b
    if (y == v->lex->define) return ana_define(v, e, m, x); // b
    if (y == v->lex->begin)  return ana_begin(v, e, m, b); // b
    if ((x = tbl_get(v, v->lex->macros, a, 0)))
      return ana_macro(v, e, m, x, b); } // b
  return ana_ap(v, e, m, a, b); } // b

static NoInline Co(em1) { return ana_i(v, e, m, (vm*) *v->sp++); }

static NoInline mo ana_i(la v, env *e, size_t m, vm *i) {
  mo k = co_pull(v, e, m + 1);
  return k ? pulli(i, k) : 0; }

static NoInline mo ana_i_x(li v, env *e, size_t m, vm *i, ob x) {
  mo k; with(x, k = co_pull(v, e, m + 2));
  return k ? pullix(i, x, k) : 0; }

static NoInline Co(em2) {
  vm *i = (vm*) *v->sp++;
  ob x = *v->sp++;
  return ana_i_x(v, e, m, i, x); }

static Co(ana_alloc) {
  mo k = mo_n(v, m + 1);
  if (k) setw(k, nil, m),
         G(k += m) = (vm*) (e ? (*e)->name : nil);
  return k; }

Vm(ev_f) {
  mo k = (mo) tbl_get(v, v->lex->topl, (ob) v->lex->eval, 0);
  return
    k && G(k) != ev_f ? ApY(k, xp) :
    !fp->argc ? ApC(ret, xp) :
    (CallOut(k = ana(v, fp->argv[0])), !k) ? Yield(OomError, xp) :
    ApY(k, xp); }
