#include "i.h"

Vm(ev_f) {
  mo k = (mo) tbl_get(v, v->lex->topl, (ob) v->lex->eval, 0);
  return
    k && G(k) != ev_f ? ApY(k, xp) :
    !fp->argc ? ApC(ret, xp) :
    (CallOut(k = ana(v, fp->argv[0])), !k) ? Yield(OomError, xp) :
    ApY(k, xp); }

// supplemental list functions
//

// index of item in list (-1 if absent)
static NoInline intptr_t lidx(ob l, ob x) {
  for (intptr_t i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

// append to tail
static NoInline two snoc(li v, ob l, ob x) {
  return !twop(l) ? pair(v, x, l) :
    (with(l, x = (ob) snoc(v, B(l), x)),
     x ? pair(v, A(l), x) : 0); }

static NoInline ob linitp(li v, ob x, ob *d) {
  ob y; return !twop(B(x)) ? (*d = x, nil) :
    (with(x, y = linitp(v, B(x), d)),
     y ? (ob) pair(v, A(x), y) : 0); }

////
///  the thread compiler
//
// this is the most complicated part of the C code but it
// normally only gets used during initialization to bootstrap the
// self-hosted compiler.
//
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1
static NoInline ob asign(li v, ob a, intptr_t i, ob *m) {
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == (ob) v->lex->splat)
    return *m = -i - 1, (ob) pair(v, A(a), nil);
  ob x; return with(a, x = asign(v, B(a), i + 1, m)),
               !x ? 0 : (ob) pair(v, A(a), x); }

// " compilation environments "
typedef struct env {
  ob arg, loc, clo, name, asig, s1, s2, s3;
  struct env *par; } *env;

static NoInline ob new_scope(li v, env *e, ob arg, ob nom) {
  intptr_t asig = 0; return
    with(nom, arg = asign(v, arg, 0, &asig)),
    !arg ? 0 : (ob) thd(v,
      arg, nil, nil, // arg loc clo
      nom, putnum(asig), // name asig
      nil, nil, nil, // s1 s2
      e ? *e : (env) nil, // par
      End); }

static mo ana_alloc(li, env*, size_t),
          p_co_x(li, env*, size_t);

static mo pull(li v, env *e, size_t m) { return
  ((mo (*)(li, env*, size_t)) (*v->sp++))(v, e, m); }

static mo pulli(vm *i, mo k) {
  return k--, G(k) = i, k; }

static mo pullix(vm *i, ob x, mo k) {
  return G(k -= 2) = i, GF(k) = (vm*) x, k; }

static mo em1(li v, env *e, size_t m) {
  vm *i = (vm*) *v->sp++;
  mo k = pull(v, e, m + 1);
  return k ? pulli(i, k) : 0; }

static mo em2(li v, env *e, size_t m) {
  vm *i = (vm*) *v->sp++;
  ob x = *v->sp++;
  mo k; with(x, k = pull(v, e, m + 2));
  return k ? pullix(i, x, k) : 0; }

static bool ana_i_x_b(li v,  vm *i, ob x) {
  return pushs(v, em2, i, x, End); }

mo ana(li v, ob x) { return
  !pushs(v, x, em1, ret, ana_alloc, End) ? NULL :
    p_co_x(v, NULL, 0); }

static NoInline ob rw_let_fn(li v, ob x) {
  mm(&x);
  for (two w; x && twop(A(x)); x =
    (w = snoc(v, BA(x), AB(x))) &&
    (w = pair(v, (ob) v->lex->lambda, (ob) w)) &&
    (w = pair(v, (ob) w, BB(x))) ?
      (ob) pair(v, AA(x), (ob) w) : 0);
  return um, x; }

static bool scan(li, env*, ob) NoInline;
static NoInline int scan_def(li v, env *e, ob x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  int r; return 
    with(x,
       r = scan_def(v, e, BB(x)),
       r = r != 1 ? r :
         !(x = rw_let_fn(v, x)) ||
         !((*e)->loc = (ob) pair(v, A(x), (*e)->loc)) ||
         !scan(v, e, AB(x)) ? -1 : 1),
    r; }

static NoInline bool scan(li v, env *e, ob x) {
  bool _; return !twop(x) ||
    A(x) == (ob) v->lex->lambda ||
    A(x) == (ob) v->lex->quote ||
    (A(x) == (ob) v->lex->define &&
     scan_def(v, e, B(x)) != -1) ||
    (with(x, _ = scan(v, e, A(x))),
     _ && scan(v, e, B(x))); }

static mo p_ana_define_bind(li v, env *e, size_t m) {
  ob _ = *v->sp++; bool ok = e ?
    ana_i_x_b(v, defsl1, putnum(lidx((*e)->loc, _))) :
    ((_ = (ob) pair(v, (ob) v->lex->topl, _)) &&
     ana_i_x_b(v, deftop, _));
  return ok ? pull(v, e, m) : 0; }

static Inline ob ana_lambda_body(la v, env *e, ob x) {
  intptr_t i;
  if (!pushs(v, p_co_x, x, em1, ret, ana_alloc, End) ||
      !scan(v, e, v->sp[1]) ||
      !(x = (ob) pull(v, e, 4)))
    return 0;
  return
    x = !(i = llen((*e)->loc)) ? x :
     (ob) pullix(setloc, putnum(i), (mo) x),
    x = (i = getnum((*e)->asig)) > 0 ?
          (ob) pullix(arity, putnum(i), (mo) x) :
        i < 0 ?
          (ob) pullix(varg, putnum(-i-1), (mo) x) :
        x,
    mo_tag((mo) x)->head = (mo) x,

    !twop((*e)->clo) ? x : (ob) pair(v, (*e)->clo, x); }

static NoInline bool ana_lambda_b_enclose(li v, env *e, ob x) {
  ob vars = A(x), code = B(x);
  size_t i = llen(vars);
  mm(&vars); mm(&code);
  vars = pushs(v, em2, take, putnum(i), ana_alloc, End) ? vars : 0;
  while (vars && i--) vars =
    pushs(v, p_co_x, A(vars), em1, push, End) ? B(vars) : 0;
  vars = vars ? (ob) pull(v, e, 0) : vars;
  vars = vars ? (ob) pair(v, code, vars) : vars;
  um, um;
  if (!vars) return false;
  vm *j = e && homp((*e)->loc) ? encl1 : encl0;
  return pushs(v, em2, j, vars, End); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).

static NoInline bool ana_lambda_b(li v, env *e, ob x) {
  ob y = nil,
     n = *v->sp == (ob) p_ana_define_bind ? v->sp[1] : nil;
  return
    with(n, with(x, with(y,
      x = (x = twop(x) ? x : (ob) pair(v, x, nil)) &&
          (x = linitp(v, x, &y)) &&
          (n = (ob) pair(v, n, e ? (*e)->name : nil)) &&
          (n = new_scope(v, e, x, n)) ?
            ana_lambda_body(v, (env*) &n, A(y)) :
            0))),
    !x ? 0 :
    twop(x) ? ana_lambda_b_enclose(v, e, x) :
    pushs(v, em2, imm, x, End); }

static NoInline bool ana_define_b_r(la v, env *e, ob x) {
  bool _; return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = ana_define_b_r(v, e, BB(x))), _) &&
     pushs(v, p_co_x, AB(x),
              p_ana_define_bind, A(x),
              End)); }

// syntactic sugar for define
// (: a b c) => (, (: a b) c)
static NoInline bool ana_define_sug(li v, two x) {
  ob _ = nil; return
    (with(_, x = (two) linitp(v, (ob) x, &_)), x) &&
    (x = pair(v, (ob) x, _)) &&
    (x = pair(v, (ob) v->lex->begin, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    (x = pair(v, (ob) v->lex->lambda, (ob) x)) &&
    (x = pair(v, (ob) x, nil)) &&
    pushs(v, p_co_x, x, End); }

static NoInline bool ana_define_b(li v, env *e, ob x) {
  ob b = B(x); return
    !twop(b) ? pushs(v, em1, imm0, End) :
    llen(b) & 1 ? ana_define_sug(v, (two) x) :
                  ana_define_b_r(v, e, b); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
static NoInline mo p_ana_cond_pre(li v, env *e, size_t m) {
  ob x = (ob) pull(v, e, m); return
    x = x ? (ob) pair(v, x, (*e)->s2) : x,
    !x ? 0 : (mo) A((*e)->s2 = x); }

// before generating a branch emit a jump to
// the top of stack 2
static mo p_ana_cond_pre_con(li v, env *e, size_t m) {
  mo x = pull(v, e, m + 2);
  if (!x) return x;
  mo k = (mo) A((*e)->s2);
  return G(k) == ret ? pulli(ret, x) :
                       pullix(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
static mo p_ana_cond_post_con(li v, env *e, size_t m) {
  ob x = (ob) pull(v, e, m); return
    x = x ? (ob) pair(v, x, (*e)->s1) : x,
    x ? (mo) A((*e)->s1 = x) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
static mo p_ana_cond_pre_ant(li v, env *e, size_t m) {
  mo x = pull(v, e, m + 2);
  return x = x ? pullix(br1, A((*e)->s1), x) : x,
         (*e)->s1 = B((*e)->s1),
         x; }

static NoInline bool ana_cond_loop_b(la v, env *e, ob x) {
  x = twop(x) ? x : (ob) pair(v, nil, nil);
  if (!x) return false;
  if (!twop(B(x))) return
    pushs(v, p_co_x, A(x), p_ana_cond_pre_con, End);
  bool _; return
    with(x,
      _ = pushs(v, p_ana_cond_post_con, p_co_x,
                   AB(x), p_ana_cond_pre_con,
                   End),
      _ = _ ? ana_cond_loop_b(v, e, BB(x)) : _),
    !_ ? 0 : pushs(v, p_co_x, A(x),
                      p_ana_cond_pre_ant,
                      End); }

static mo p_ana_cond_post(li v, env *e, size_t m) {
  mo k = pull(v, e, m);
  return (*e)->s2 = B((*e)->s2), k; }

static NoInline bool ana_cond_b(li v, env *e, ob x) {
  bool _; return
    (with(x, _ = pushs(v, p_ana_cond_pre, End)), _) &&
    ana_cond_loop_b(v, e, x) &&
    pushs(v, p_ana_cond_post, End); }

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

static NoInline bool ana_sym_lazy_b(li v, env *e, ob x, ob b) {
  return (x = (ob) pair(v, b, x)) &&
    pushs(v, em2, late, x, End); }

static NoInline bool ana_sym_ref_b(li v, env *e, ob x, ob a) {
  ob idx = putnum(lidx(((ob*)(*e))[a], x));
  vm *i = a == Arg ? argn : a == Clo ? clon : sl1n;
  return ana_i_x_b(v, i, idx); }

static NoInline bool ana_sym_clo_b(li v, env *e, ob x) {
  size_t y = llen((*e)->clo);
  if (!(x = (ob) snoc(v, (*e)->clo, x))) return false;
  (*e)->clo = x;
  return ana_i_x_b(v, clon, putnum(y)); }

static NoInline bool ana_sym_b(li v, env *e, ob x) {
  ob b; enum where a =
    ana_sym_look(v, e ? *e : (env) nil, x, &b);
  if (a == Here) return ana_i_x_b(v, imm, b);
  if (a == Wait) return ana_sym_lazy_b(v, e, x, b);
  if (b == (ob) *e) return ana_sym_ref_b(v, e, x, a);
  return ana_sym_clo_b(v, e, x); }

static bool ana_list_b(li, env*, ob);
static bool ana_x_b(li v, env *e, ob x) {
  return symp(x) ? ana_sym_b(v, e, x) :
         twop(x) ? ana_list_b(v, e, x) :
         ana_i_x_b(v, imm, x); }

static NoInline mo p_co_x(li v, env *e, size_t m) {
  return ana_x_b(v, e, *v->sp++) ? pull(v, e, m) : 0; }

static mo p_ana_ap_call(li v, env *e, size_t m) {
  ob ary = *v->sp++;
  mo k = pull(v, e, m + 2);
  return !k ? k :
    G(k) == ret ? pullix(rec, ary, k + 1) :
    pullix(call, ary, k); }

static NoInline bool ana_ap_b(li v, ob f, ob x) {
  mm(&x);
  bool ok = pushs(v,
    p_co_x, f,
    em1, idmo,
    p_ana_ap_call, putnum(llen(x)),
    End);
  for (; ok && twop(x); x = B(x)) ok =
    pushs(v, p_co_x, A(x), em1, push, End);
  return um, ok; }


static NoInline bool ana_begin_loop(la v, env *e, ob x) {
  bool _; return !twop(x) ||
    (with(x, _ = ana_begin_loop(v, e, B(x))),
     _ && pushs(v, p_co_x, A(x), End)); }

static NoInline bool ana_begin_b(li v, env *e, ob x) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x && ana_begin_loop(v, e, x); }

static enum status li_ap(la v, mo f, ob x) {
  mo k = thd(v,
    immp, x,
    immp, f,
    imm, nil, // assignment target idx=5
    call, putnum(2),
    xok, ap_f, // source idx=9
    End);
  return !k ? OomError :
    (k[5].ap = (vm*) (k + 9),
     v->ip = k,
     li_go(v)); }

static NoInline bool ana_macro_b(li v, ob mac, ob x) {
  ob xp = v->xp;
  mo ip = v->ip;
  enum status s; return
    with(xp, with(ip, s = li_ap(v, (mo) mac, x))),
    x = v->xp,
    v->xp = xp,
    v->ip = ip,
    s != Ok ? (report(v, s), false) : pushs(v, p_co_x, x, End); }

static bool ana_list_b(li v, env *e, ob x) {
  ob a = A(x), b = B(x);
  if (symp(a)) {
    sym y = (sym) a;
    if (y == v->lex->quote) return
      ana_i_x_b(v, imm, twop(b) ? A(b) : b);
    if (y == v->lex->cond)   return ana_cond_b(v, e, b);
    if (y == v->lex->lambda) return ana_lambda_b(v, e, b);
    if (y == v->lex->define) return ana_define_b(v, e, x);
    if (y == v->lex->begin)  return ana_begin_b(v, e, b);
    if ((x = tbl_get(v, v->lex->macros, a, 0)))
      return ana_macro_b(v, x, b); }
  return ana_ap_b(v, a, b); }

static mo ana_alloc(li v, env *e, size_t m) {
  mo k = mo_n(v, m + 1);
  if (k) setw(k, nil, m),
         G(k += m) = (vm*) (e ? (*e)->name : nil);
  return k; }
