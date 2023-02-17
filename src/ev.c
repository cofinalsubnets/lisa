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

// return the init of a list and store a pointer to its last pair.
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

static bool
            ana_i_x_b(li, vm*, ob);
static mo
  ana_cond(li, env*, size_t, ob),
  ana_lambda(li, env*, size_t, ob),
  ana_let(li, env*, size_t, ob),
  ana_begin(li, env*, size_t, ob),
  ana_macro(li, env*, size_t, ob, ob),
  ana_ap(li, env*, size_t, ob, ob);


static Inline bool toplp(env *e) { return !e; }
static NoInline env new_scope(li v, env *e, ob arg, ob nom) {
  intptr_t asig = 0; return
    with(nom, arg = asign(v, arg, 0, &asig)),
    !arg ? 0 : (env) thd(v,
      arg, nil, nil, // arg loc clo
      nom, putnum(asig), // name asig
      nil, nil, nil, // s1 s2 s3
      toplp(e) ? (env) nil : *e, // par
      End); }

static mo ana_alloc(li, env*, size_t);

static Inline mo pull(li v, env *e, size_t m) { return
  ((mo (*)(li, env*, size_t)) (*v->sp++))(v, e, m); }

static Inline mo pulli(vm *i, mo k) {
  return k--, G(k) = i, k; }

static Inline mo pullix(vm *i, ob x, mo k) {
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

static NoInline bool ana_i_x_b_gc(li v, vm *i, ob x) {
  bool _; with(x, _ = please(v, 3));
  return _ && ana_i_x_b(v, i, x); }

static NoInline bool ana_i_x_b(li v,  vm *i, ob x) {
  if (Avail < 3) return ana_i_x_b_gc(v, i, x);
  ob *s = v->sp -= 3; return
    s[0] = (ob) em2, s[1] = (ob) i, s[2] = x,
    true; }

static NoInline mo ana_i_x(li v, env *e, size_t m, vm *i, ob x) {
  mo k; with(x, k = pull(v, e, m + 2));
  return pullix(i, x, k); }

static mo ana_sym(li, env*, size_t, ob),
          ana_two(li, env*, size_t, ob);

static mo ana_x(li v, env *e, size_t m, ob x) { return
  symp(x) ? ana_sym(v, e, m, x) :
  twop(x) ? ana_two(v, e, m, x) :
  ana_i_x(v, e, m, imm, x); }

static mo p_ana_x(li v, env *e, size_t m) {
  return ana_x(v, e, m, *v->sp++); }

mo ana(li v, ob x) { return
  pushs(v, p_ana_x, x, em1, ret, ana_alloc, End) ?
    pull(v, NULL, 0) : 0; }

// (: (((a b) c) d) x) => (: a (\ b (\ c (\ d x))))
static NoInline ob rw_let_fn(li v, ob x) {
  mm(&x);
  for (two w; x && twop(A(x)); x =
    (w = snoc(v, BA(x), AB(x))) &&
    (w = pair(v, (ob) v->lex->lambda, (ob) w)) &&
    (w = pair(v, (ob) w, BB(x))) ?
      (ob) pair(v, AA(x), (ob) w) : 0);
  return um, x; }

static bool add_loc(li v, env *e, ob y) {
  if (!e || lidx((*e)->loc, y) > -1) return true;
  if (!(y = (ob) pair(v, y, (*e)->loc))) return false;
  return (*e)->loc = y, true; }

static Inline ob ana_lambda_body(la v, env *e, ob x) {
  intptr_t i;
  if (!pushs(v, p_ana_x, x, em1, ret, ana_alloc, End) ||
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

static NoInline mo ana_lambda_enclose(li v, env *e, size_t m, ob vars, ob code, mo k) {
  size_t i = llen(vars);
  mm(&vars); mm(&code); mm(&k);
  vars = pushs(v, em2, take, putnum(i), ana_alloc, End) ? vars : 0;
  while (vars && i--) vars =
    pushs(v, p_ana_x, A(vars), em1, push, End) ?
      B(vars) : 0;
  vars = vars ? (ob) pull(v, e, 0) : vars;
  vars = vars ? (ob) pair(v, code, vars) : vars;
  vm *j = e && !nilp((*e)->loc) ? encl1 : encl0;
  k = vars ? pullix(j, vars, k) : 0;
  return um, um, um, k; }

static NoInline mo ana_let_bind_top(li v, env *e, size_t m, ob _) {
  return (_ = (ob) pair(v, (ob) v->lex->topl, _)) &&
    ana_i_x_b(v, deftop, _) ?
    pull(v, e, m) : 0; }

static NoInline mo ana_let_bind_inner(li v, env *e, size_t m, ob _) {
  mo k; with(_,
    k = add_loc(v, e, _) ? pull(v, e, m + 2) : 0);
  intptr_t i = lidx((*e)->loc, _);
  return !k ? k : pullix(defsl1, putnum(i), k); }

static mo p_ana_let_bind(li v, env *e, size_t m) {
  ob _ = *v->sp++; return toplp(e) ?
    ana_let_bind_top(v, e, m, _) :
    ana_let_bind_inner(v, e, m, _); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).

static NoInline mo ana_lambda(li v, env *e, size_t m, ob x) {
  ob y = nil,
     n = v->sp[0] == (ob) p_ana_let_bind ? v->sp[1] : nil;
  mo k; with(x, with(n,
    k = pull(v, e, m + 2),
    with(k, with(y, x =
     k &&
     (x = twop(x) ? x : (ob) pair(v, x, nil)) &&
     (x = linitp(v, x, &y)) &&
     (n = (ob) pair(v, n, toplp(e) ? nil : (*e)->name)) &&
     (n = (ob) new_scope(v, e, x, n)) ?
       ana_lambda_body(v, (env*) &n, A(y)) :
       0))));
  return !x ? 0 :
    twop(x) ? ana_lambda_enclose(v, e, m, A(x), B(x), k) :
              pullix(imm, x, k); }

static NoInline bool ana_let_b_even(li v, env *e, ob x) {
  bool _; return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = ana_let_b_even(v, e, BB(x))), _) &&
     pushs(v, p_ana_x, AB(x), p_ana_let_bind, A(x), End)); }

static mo p_ana_let_right(li v, env *e, size_t m) {
  return pull(v, e, m); }

static mo p_ana_let_left(li v, env *e, size_t m) {
  return pull(v, e, m); }

// (: a b c) => (, (: a b) c)
static NoInline bool ana_let_b_odd(li v, env *e, ob x) {
  ob _ = nil; return
    pushs(v, x, p_ana_let_right, End) &&
    (with(_,
       x = linitp(v, *v->sp++, &_),
       x = x ? (ob) pair(v, (ob) v->lex->define, x) : x),
     x) &&
    (x = (ob) pair(v, x, _)) &&
    (x = (ob) pair(v, (ob) v->lex->begin, x)) &&
    (x = (ob) pair(v, x, nil)) &&
    (x = (ob) pair(v, (ob) v->lex->lambda, x)) &&
    (x = (ob) pair(v, x, nil)) &&
    pushs(v, p_ana_let_left, p_ana_x, x, End); }

static NoInline mo ana_let(li v, env *e, size_t m, ob x) {
  bool ok =
    !twop(x) ? pushs(v, em1, imm, End) :
    llen(x) & 1 ? ana_let_b_odd(v, e, x) :
                  ana_let_b_even(v, e, x);
  return ok ? pull(v, e, m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
static NoInline mo p_ana_cond_pre(li v, env *e, size_t m) {
  ob x = (ob) pull(v, e, m); return
    x = x ? (ob) pair(v, x, (*e)->s2) : x,
    x ? (mo) A((*e)->s2 = x) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
static mo p_ana_cond_pre_con(li v, env *e, size_t m) {
  mo x = pull(v, e, m + 2), k; return !x ? x :
    G(k = (mo) A((*e)->s2)) == ret ?
      pulli(ret, x) :
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
    pushs(v, p_ana_x, A(x), p_ana_cond_pre_con, End);
  bool _; return
    with(x,
      _ = pushs(v,
        p_ana_cond_post_con,
        p_ana_x, AB(x),
        p_ana_cond_pre_con,
        End),
      _ = _ ? ana_cond_loop_b(v, e, BB(x)) : _),
    _ && pushs(v, p_ana_x, A(x), p_ana_cond_pre_ant, End); }

static mo p_ana_cond_post(li v, env *e, size_t m) {
  mo k = pull(v, e, m);
  return (*e)->s2 = B((*e)->s2), k; }

static NoInline mo ana_cond(li v, env *e, size_t m, ob x) {
  bool _; return
    with(x, _ = pushs(v, p_ana_cond_pre, End)),
    _ && ana_cond_loop_b(v, e, x) &&
         pushs(v, p_ana_cond_post, End) ? pull(v, e, m) : 0; }

         /*
static ob getnym(env e, ob y) {
  for (ob i = e->s3; twop(i); i = B(i))
    for (ob j = A(i); twop(j); j = B(j))
      if (AA(j) == y) return BA(j);
  return 0; }
  */

enum where { Arg, Loc, Clo, Here, Wait };
static NoInline enum where ana_sym_look(li v, env e, ob y, ob *p) {
  if (nilp((ob) e)) return // toplevel
    (y = tbl_get(v, v->lex->topl, y, 0)) ?
      (*p = y, Here) :
      (*p = (ob) v->lex->topl, Wait);

//  ob nym = getnym(e, y);
//  if (nym && lidx(e->loc, y) >= 0) return (*p = (ob) e, Loc);

  return
    lidx(e->loc, y) >= 0 ? (*p = (ob) e, Loc) :
    lidx(e->arg, y) >= 0 ? (*p = (ob) e, Arg) :
    lidx(e->clo, y) >= 0 ? (*p = (ob) e, Clo) :
    ana_sym_look(v, (env) e->par, y, p); }

static mo ana_sym(li v, env *e, size_t m, ob x) {
  mo k; with(x, k = pull(v, e, m + 2));
  if (!k) return k;
  ob b; enum where a =
    ana_sym_look(v, !toplp(e) ? *e : (env) nil, x, &b);
  if (a == Here)
    return pullix(imm, b, k);
  if (a == Wait) return
    with(k, x = (ob) pair(v, b, x)),
    x ? pullix(late, x, k) : 0;
  if (b == (ob) *e) {
    ob idx = putnum(lidx(((ob*)(*e))[a], x));
    vm *i = a == Arg ? argn : a == Clo ? clon : sl1n;
    return pullix(i, idx, k); }
  size_t y = llen((*e)->clo);
  with(k, x = (ob) snoc(v, (*e)->clo, x));
  if (!x) return 0;
  return (*e)->clo = x,
         pullix(clon, putnum(y), k); }

static mo p_ana_ap_call(li v, env *e, size_t m) {
  ob ary = *v->sp++;
  mo k = pull(v, e, m + 2);
  return !k ? k :
    G(k) == ret ? pullix(rec, ary, k + 1) :
    pullix(call, ary, k); }

static NoInline mo ana_ap(li v, env *e, size_t m, ob f, ob x) {
  mm(&x);
  bool ok = pushs(v,
    p_ana_x, f,
    em1, idmo,
    p_ana_ap_call, putnum(llen(x)),
    End);
  for (; ok && twop(x); x = B(x))
    ok = pushs(v, p_ana_x, A(x), em1, push, End);
  return um, ok ? pull(v, e, m) : 0; }

static NoInline bool ana_begin_b_loop(la v, env *e, ob x) {
  bool _; return !twop(x) ||
    (with(x, _ = ana_begin_b_loop(v, e, B(x))),
     _ && pushs(v, p_ana_x, A(x), End)); }

static NoInline mo ana_begin(li v, env *e, size_t m, ob x) {
  return x = twop(x) ? x : (ob) pair(v, x, nil),
         x && ana_begin_b_loop(v, e, x) ?
           pull(v, e, m) : 0; }

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

static NoInline mo ana_macro(li v, env *e, size_t m, ob mac, ob x) {
  ob xp = v->xp;
  mo ip = v->ip;
  enum status s;
  with(xp, with(ip, s = li_ap(v, (mo) mac, x)));
  if (s != Ok) return report(v, s), NULL;
  return
    x = v->xp,
    v->xp = xp,
    v->ip = ip,
    pushs(v, p_ana_x, x, End) ? pull(v, e, m) : 0; }

static NoInline mo ana_two(li v, env *e, size_t m, ob x) {
  ob a = A(x), b = B(x);
  if (symp(a)) {
    sym y = (sym) a;
    if (y == v->lex->quote) return
      ana_i_x(v, e, m, imm, twop(b) ? A(b) : b);
    if (y == v->lex->cond)   return ana_cond(v, e, m, b);
    if (y == v->lex->lambda) return ana_lambda(v, e, m, b);
    if (y == v->lex->define) return ana_let(v, e, m, b);
    if (y == v->lex->begin)  return ana_begin(v, e, m, b);
    if ((x = tbl_get(v, v->lex->macros, a, 0)))
      return ana_macro(v, e, m, x, b); }
  return ana_ap(v, e, m, a, b); }

static mo ana_alloc(li v, env *e, size_t m) {
  mo k = mo_n(v, m + 1);
  if (k) setw(k, nil, m),
         G(k += m) = (vm*) (toplp(e) ? nil : (*e)->name);
  return k; }
