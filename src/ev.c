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

typedef mo co(la, env*, size_t);

static mo
  em1(la, env*, size_t),
  em2(la, env*, size_t),
  mo_alloc(la, env*, size_t),
  p_co_x(la, env*, size_t),
  co_def(la, env*, size_t, ob) NoInline,
  co_fn(la, env*, size_t, ob) NoInline,
  mo_mac(la, env*, size_t, ob, ob) NoInline,
  mo_two(la, env*, size_t, ob) NoInline,
  mo_i_x(la, env*, size_t, vm*, ob) NoInline;

static mo ana(la v, ob x) { return
  !pushs(v, x, em1, ret, mo_alloc, NULL) ? 0 :
    p_co_x(v, 0, 0); }

Vm(ev_f) {
  mo e = (mo) tbl_get(v, v->lex.topl, (ob) v->lex.eval, 0);
  return
    e && G(e) != ev_f ? ApY(e, xp) :
    !fp->argc ? ApC(ret, xp) :
    (CallOut(e = ana(v, fp->argv[0])), !e) ? Yield(OomError, xp) :
    ApY(e, xp); }

static Inline mo co_pull(la v, env *e, size_t m) { return
  ((mo (*)(la, env*, U)) (*v->sp++))(v, e, m); }


// apply instruction pullbacks
static Inline mo pulli(vm *i, mo k) {
  return k--, G(k) = i, k; }
static Inline mo pullix(vm *i, ob x, mo k) {
  return pulli(i, pulli((vm*) x, k)); }

// supplemental list functions
//

// index of item in list (-1 if absent)
static NoInline intptr_t lidx(ob l, ob x) {
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
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == (ob) v->lex.splat)
    return *m = -i - 1, (ob) pair(v, A(a), nil);
  ob x; with(a, x = asign(v, B(a), i + 1, m));
  return !x ? 0 : (ob) pair(v, A(a), x); }

static ob new_scope(la v, env *e, ob arg, ob nom) {
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

static bool scan(la, env*, ob) NoInline;
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
  ob y; with(x, y = linitp(v, B(x), d));
  return y ? (ob) pair(v, A(x), y) : 0; }

static Inline ob comp_body(la v, env *e, ob x) {
  I i;
  if (!pushs(v, p_co_x, x, em1, ret, mo_alloc, NULL) ||
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
  if (!twop((*e)->clo)) return x;
  return (ob) pair(v, (*e)->clo, x); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static ob co_fn_ltu(la v, env *e, ob n, ob l) {
  ob y = nil;
  with(n, with(y, with(l,
    l = (l = twop(l) ? l : (ob) pair(v, l, nil)) &&
        (l = linitp(v, l, &y)) &&
        (n = (ob) pair(v, n, e ? (*e)->name : nil)) &&
        (n = new_scope(v, e, l, n)) ?
          comp_body(v, (env*) &n, A(y)) :
          0)));
  return l; }

static ob co_fn_clo(la v, env *e, ob vars, ob code) {
  U i = llen(vars);
  mm(&vars), mm(&code);

  vars = pushs(v, em2, take, putnum(i), mo_alloc, NULL) ? vars : 0;
  while (vars && i--) vars =
    pushs(v, p_co_x, A(vars), em1, push, NULL) ? B(vars) : 0;
  vars = vars ? (ob) co_pull(v, e, 0) : vars;
  vars = vars ? (ob) pair(v, code, vars) : vars;
  return um, um, vars; }

#define Co(nom,...)\
  mo nom(la v, env *e, size_t m, ##__VA_ARGS__)

static Co(co_fn_enclose, ob x, mo k) {
  with(k, x = co_fn_clo(v, e, A(x), B(x)));
  if (!x) return 0;
  return pullix(e && homp((*e)->loc) ? encl1 : encl0, x, k); }

static Co(p_co_def_bind) {
  ob _ = *v->sp++;
  if (!e) return
    _ = (ob) pair(v, (ob) v->lex.topl, _),
    _ ? mo_i_x(v, e, m, deftop, _) : 0;
  return mo_i_x(v, e, m, defsl1, putnum(lidx((*e)->loc, _))); }

static Co(co_fn, ob x) {
  ob nom = *v->sp == (ob) p_co_def_bind ? v->sp[1] : nil;
  mo k; with(nom, with(x, k = co_pull(v, e, m+2)));
  if (!k) return 0;
  with(k, x = co_fn_ltu(v, e, nom, x));
  if (!x) return 0;
  if (G(x) == act) return co_fn_enclose(v, e, m, x, k);
  return pullix(imm, x, k); }

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

static Co(co_def, ob x) { return
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
static Co(co_if_pre) {
  ob x = (ob) co_pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s2) : x;
  return x ? (mo) A((*e)->s2 = x) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
static Co(co_if_pre_con) {
  mo k, x = co_pull(v, e, m + 2);
  return !x ? 0 : G(k = (mo) A((*e)->s2)) == ret ?
    pulli(ret, x) : pullix(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
static Co(co_if_post_con) {
  ob x = (ob) co_pull(v, e, m);
  x = x ? (ob) pair(v, x, (*e)->s1) : x;
  return x ? (mo) A((*e)->s1 = x) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
static Co(co_if_pre_ant) {
  mo x = co_pull(v, e, m + 2);
  if (!x) return 0;
  x = pullix(br1, A((*e)->s1), x);
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

static Co(co_if, ob x) {
  bool _;
  with(x, _ = pushs(v, co_if_pre, NULL));
  _ = _ && co_if_loop(v, e, x);
  if (!_) return 0;
  mo pf = co_pull(v, e, m);
  if (pf) (*e)->s2 = B((*e)->s2);
  return pf; }

static Co(p_mo_ap_call) {
  ob ary = *v->sp++;
  mo k = co_pull(v, e, m + 2);
  return k ? pullix(G(k) == ret ? rec : call, ary, k) : 0; }

enum where { Arg, Loc, Clo, Here, Wait };

static NoInline ob co_sym_look(la v, env e, ob y) { return
  nilp((ob) e) ?
    (y = tbl_get(v, v->lex.topl, y, 0)) ?
      (ob) pair(v, Here, y) :
      (ob) pair(v, Wait, (ob) v->lex.topl) :
  lidx(e->loc, y) >= 0 ? (ob) pair(v, Loc, (ob) e) :
  lidx(e->arg, y) >= 0 ? (ob) pair(v, Arg, (ob) e) :
  lidx(e->clo, y) >= 0 ? (ob) pair(v, Clo, (ob) e) :
  co_sym_look(v, (env) e->par, y); }

static Co(co_sym, ob x) {
  ob q;
  with(x, q = co_sym_look(v, e ? *e : (env) nil, x));
  if (!q) return 0;
  if (A(q) == Here) return mo_i_x(v, e, m, imm, B(q));
  if (A(q) == Wait) return
    (x = (ob) pair(v, B(q), x)) &&
    (with(x, q = (ob) co_pull(v, e, m + 2)), q) ?
      pullix(late, x, (mo) q) : 0;

  if (B(q) == (ob) *e) {
    ob idx = putnum(lidx(((ob*)(*e))[A(q)], x));
    vm *i = A(q) == Arg ? argn : A(q) == Loc ? sl1n : clon;
    return mo_i_x(v, e, m, i, idx); }

  U y = llen((*e)->clo);
  if (!(q = (ob) snoc(v, (*e)->clo, x))) return 0;
  return (*e)->clo = q, mo_i_x(v, e, m, clon, putnum(y)); }

static Co(co_x, ob x) { return
  symp(x) ? co_sym(v, e, m, x) :
  twop(x) ? mo_two(v, e, m, x) :
  mo_i_x(v, e, m, imm, x); }

static Co(p_co_x) { return co_x(v, e, m, *v->sp++); }

static Co(mo_ap, ob f, ob args) {
  mm(&args);
  if (!pushs(v,
        p_co_x, f,
        em1, idmo,
        p_mo_ap_call, putnum(llen(args)),
        NULL))
    return um, NULL;
  for (; twop(args); args = B(args))
    if (!pushs(v, p_co_x, A(args), em1, push, NULL))
      return um, NULL;
  return um, co_pull(v, e, m); }

static NoInline bool seq_mo_twooop(la v, env *e, ob x) {
  bool _; return !twop(x) ||
    (with(x, _ = seq_mo_twooop(v, e, B(x))),
     _ && pushs(v, p_co_x, A(x), NULL)); }

static Co(mo_seq, ob x) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x && seq_mo_twooop(v, e, x) ?
    co_pull(v, e, m) :
    0; }

static enum status li_ap(la v, mo f, ob x) {
  mo k = thd(v,
    imm, x, push,
    imm, f, push,
    imm, nil, // assignment target idx=7
    call, putnum(2),
    xok, ap_f, // source idx=11
    NULL);
  if (!k) return OomError;
  return k[7].ap = (vm*) (k + 11),
         v->ip = k,
         li_go(v); }

static Co(mo_mac, ob mac, ob x) {
  enum status s;
  ob xp = v->xp;
  mo ip = v->ip;
  return
    with(xp, with(ip, s = li_ap(v, (mo) mac, x))),
    x = v->xp, v->xp = xp, v->ip = ip,
    report(v, s), // FIXME should return status
    s == Ok ? co_x(v, e, m, x) : NULL; }

static Co(mo_two, ob x) {
  ob a = A(x);
  if (symp(a)) {
    sym y = (sym) a;
    if (y == v->lex.quote) return
      mo_i_x(v, e, m, imm, twop(B(x)) ? AB(x) : B(x));
    if (y == v->lex.cond)   return co_if(v, e, m, B(x));
    if (y == v->lex.lambda) return co_fn(v, e, m, B(x));
    if (y == v->lex.define) return co_def(v, e, m, x);
    if (y == v->lex.begin)  return mo_seq(v, e, m, B(x)); }
  if ((a = tbl_get(v, v->lex.macros, a, 0)))
    return mo_mac(v, e, m, a, B(x));
  return mo_ap(v, e, m, A(x), B(x)); }

static Co(em1) {
  vm *i = (vm*) *v->sp++;
  mo k = co_pull(v, e, m + 1);
  return k ? pulli(i, k): 0; }

static mo mo_i_x(la v, env *e, size_t m, vm *i, ob x) {
  mo k; return
    with(x, k = co_pull(v, e, m + 2)),
    k ? pullix(i, x, k) : 0; }

static Co(em2) {
  vm *i = (vm*) *v->sp++;
  ob x = *v->sp++;
  return mo_i_x(v, e, m, i, x); }

static Co(mo_alloc) {
  mo k = mo_n(v, m + 1);
  if (k) setw(k, nil, m),
         G(k += m) = (vm*) (e ? (*e)->name : nil);
  return k; }
