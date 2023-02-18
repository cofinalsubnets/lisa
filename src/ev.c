#include "i.h"

////
///  the thread compiler
//
// this is the most complicated part of the C code but it
// normally only gets used during initialization to bootstrap the
// self-hosted compiler.
//
// " compilation environments "
typedef struct env {
  ob arg, loc, clo, name, asig, s1, s2, s3;
  struct env *par; } *env;
//
// if a function is not variadic its arity signature is
// n = number of required arguments; otherwise it is -n-1
static NoInline ob asign(li v, ob a, intptr_t i, ob *m) {
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == (ob) v->lex->splat)
    return *m = -i - 1, (ob) pair(v, A(a), nil);
  ob x; return with(a, x = asign(v, B(a), i + 1, m)),
               !x ? 0 : (ob) pair(v, A(a), x); }

static Inline bool toplp(env e) { return nilp((ob) e->par); }
static mo p_alloc(li, env*, size_t),
          ana_if(li, env*, size_t, ob),
          ana_fn(li, env*, size_t, ob),
          ana_let(li, env*, size_t, ob),
          ana_seq(li, env*, size_t, ob),
          ana_mac(li, env*, size_t, ob, ob),
          ana_ap(li, env*, size_t, ob, ob),
          ana_sym(li, env*, size_t, ob),
          ana_two(li, env*, size_t, ob, ob);

static Inline mo pull(li v, env *e, size_t m) { return
  ((mo (*)(li, env*, size_t)) (*v->sp++))(v, e, m); }

static Inline mo pulli(vm *i, mo k) {
  return k--, G(k) = i, k; }

static Inline mo pullix(vm *i, ob x, mo k) {
  return G(k -= 2) = i, GF(k) = (vm*) x, k; }

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

static mo emi(li v, env *e, size_t m) {
  vm *i = (vm*) *v->sp++;
  mo k = pull(v, e, m + 1);
  return k ? pulli(i, k) : 0; }

static mo emix(li v, env *e, size_t m) {
  vm *i = (vm*) *v->sp++;
  ob x = *v->sp++;
  mo k; with(x, k = pull(v, e, m + 2));
  return k ? pullix(i, x, k) : 0; }

static NoInline mo ana_i_x(li v, env *e, size_t m, vm *i, ob x) {
  mo k; with(x, k = pull(v, e, m + 2));
  return pullix(i, x, k); }

static mo p_ana_x(li v, env *e, size_t m) {
  ob x = *v->sp++; return
    symp(x) ? ana_sym(v, e, m, x) :
    twop(x) ? ana_two(v, e, m, A(x), B(x)) :
              ana_i_x(v, e, m, imm, x); }

static mo ana(li v, ob x) {
  if (!pushs(v, x, emi, ret, p_alloc, End)) return 0;
  env e = (env) mo_n(v, Width(struct env));
  if (e) setw(e, nil, Width(struct env));
  mo k; with(e, k = p_ana_x(v, &e, 0));
  return k; }

Vm(ev_f) {
  mo k = (mo) tbl_get(v, v->lex->topl, (ob) v->lex->eval, 0);
  return k && G(k) != ev_f ? ApY(k, xp) :
         !fp->argc ? ApC(ret, xp) :
         (CallOut(k = ana(v, fp->argv[0])), !k) ?
           Yield(OomError, xp) :
         ApY(k, xp); }

// (: (((a b) c) d) x) => (: a (\ b (\ c (\ d x))))
static NoInline ob rw_let_fn(li v, ob x) {
  mm(&x);
  for (two w; x && twop(A(x)); x =
    (w = snoc(v, BA(x), AB(x))) &&
    (w = pair(v, (ob) v->lex->lambda, (ob) w)) &&
    (w = pair(v, (ob) w, BB(x))) ?
      (ob) pair(v, AA(x), (ob) w) : 0);
  return um, x; }

static NoInline mo ana_fn_clo(li v, env *e, ob vars, ob code, mo k) {
  for (mm(&k), mm(&code), mm(&vars),
       vars = pushs(v, emix, take, putnum(llen(vars)),
                       p_alloc, End) ? vars : 0;
       vars && twop(vars);
       vars = pushs(v, p_ana_x, A(vars),
                       emi, push, End) ?  B(vars) : 0);
  return
    um, vars = vars ? (ob) pull(v, e, 0) : vars,
    um, vars = vars ? (ob) pair(v, code, vars) : vars,
    um, !vars ? 0 :
      pullix(e && !nilp((*e)->loc) ? encl1 : encl0, vars, k); }

static NoInline mo ana_let_bind_top(li v, env *e, size_t m, ob _) {
  return _ = (ob) pair(v, (ob) v->lex->topl, _),
         !_ ? 0 : ana_i_x(v, e, m, deftop, _); }

static NoInline mo ana_let_bind_inner(li v, env *e, size_t m, ob _) {
  ob y; mo k; with(_, k =
    toplp(*e) || lidx((*e)->loc, _) > -1 ||
    (y = (ob) pair(v, _, (*e)->loc),
     (*e)->loc = y) ? pull(v, e, m + 2) : 0);
  intptr_t i = lidx((*e)->loc, _);
  return !k ? k : pullix(defsl1, putnum(i), k); }

static mo p_ana_let_bind(li v, env *e, size_t m) {
  ob _ = *v->sp++; return toplp(*e) ?
    ana_let_bind_top(v, e, m, _) :
    ana_let_bind_inner(v, e, m, _); }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static NoInline mo ana_fn(li v, env *e, size_t m, ob x) {
  intptr_t i = 0; mo k;
  ob y = nil,
     n = v->sp[0] == (ob) p_ana_let_bind ? v->sp[1] : nil;
  return
    with(x, with(n,
      k = pull(v, e, m + 2),
      with(k, with(y, x = k &&
        (x = twop(x) ? x : (ob) pair(v, x, nil)) &&
        (x = linitp(v, x, &y)) &&
        (n = (ob) pair(v, n, (*e)->name)) &&
        pushs(v, p_ana_x, A(y), emi, ret, p_alloc, End) &&
        (x = asign(v, x, 0, &i)) &&
        (n = (ob) thd(v, x, nil, nil, // arg loc clo
                         n, putnum(i), // nom asig
                         nil, nil, nil, // s1 s2 s3
                         *e, // par
                         End)) &&
        (x = (ob) pull(v, (env*) &n, 4)) ?
        (x = !(i = llen(((env)n)->loc)) ? x :
           (ob) pullix(setloc, putnum(i), (mo) x),
         x = (i = getnum(((env)n)->asig)) > 0 ?
               (ob) pullix(arity, putnum(i), (mo) x) :
             i < 0 ?
               (ob) pullix(varg, putnum(-i-1), (mo) x) :
             x,
         mo_tag((mo) x)->head = (mo) x,
         !twop(((env)n)->clo) ? x : (ob) pair(v, ((env)n)->clo, x)) : 0)))),
    !x ? 0 : twop(x) ? ana_fn_clo(v, e, A(x), B(x), k) :
                       pullix(imm, x, k); }

static NoInline bool ana_let_b_even(li v, env *e, ob x) {
  bool _; return !twop(x) ||
    ((x = rw_let_fn(v, x)) &&
     (with(x, _ = ana_let_b_even(v, e, BB(x))), _) &&
     pushs(v, p_ana_x, AB(x), p_ana_let_bind, A(x), End)); }

static mo p_ana_let_right(li v, env *e, size_t m) {
  ob x = *v->sp++, nyms = nil;
  mm(&nyms); mm(&x);
  for (ob y; nyms && twop(x); nyms = y, x = B(x))
    y = (ob) nym(v),
    y = y ? (ob) pair(v, A(x), y) : y,
    y = y ? (ob) pair(v, y, nyms) : y;
  um, nyms = nyms ? (ob) pair(v, nyms, (*e)->s3) : nyms;
  mo k = pull(v, e, m);
  return um, ((*e)->s3 = nyms) ? k : 0; }

static mo p_ana_let_left(li v, env *e, size_t m) {
  mo k = pull(v, e, m); return !k ? k :
    ((*e)->s3 = B((*e)->s3), k); }

// (: a b c) => (, (: a b) c)
static NoInline bool ana_let_b_odd(li v, env *e, ob x) {
  ob _ = nil; return
    with(_, x =
      (x = linitp(v, x, &_)) &&
      pushs(v, p_ana_let_right, x, End) ?
        (ob) pair(v, (ob) v->lex->define, v->sp[1]) : 0),
    x &&
    (x = (ob) pair(v, x, _)) &&
    (x = (ob) pair(v, (ob) v->lex->begin, x)) &&
    (x = (ob) pair(v, x, nil)) &&
    (x = (ob) pair(v, (ob) v->lex->lambda, x)) &&
    (x = (ob) pair(v, x, nil)) &&
    pushs(v, p_ana_let_left, p_ana_x, x, End); }

static NoInline mo ana_let(li v, env *e, size_t m, ob x) {
  bool ok = !twop(x) ? pushs(v, emi, imm, End) :
            llen(x) & 1 ? ana_let_b_odd(v, e, x) :
                          ana_let_b_even(v, e, x);
  return ok ? pull(v, e, m) : 0; }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
static NoInline mo p_ana_if_push_out(li v, env *e, size_t m) {
  ob x = (ob) pull(v, e, m); return
    x = x ? (ob) pair(v, x, (*e)->s2) : x,
    x ? (mo) A((*e)->s2 = x) : 0; }

// before generating a branch emit a jump to
// the top of stack 2
static mo p_ana_if_peek_out(li v, env *e, size_t m) {
  mo x = pull(v, e, m + 2), k; return !x ? x :
    G(k = (mo) A((*e)->s2)) == ret ? pulli(ret, x) :
                                     pullix(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
static mo p_ana_if_push_con(li v, env *e, size_t m) {
  ob x = (ob) pull(v, e, m); return
    x = x ? (ob) pair(v, x, (*e)->s1) : x,
    x ? (mo) A((*e)->s1 = x) : 0; }

// before generating an antecedent emit a branch to
// the top of stack 1
static mo p_ana_if_pop_con(li v, env *e, size_t m) {
  mo x = pull(v, e, m + 2); return !x ? x :
    (x = pullix(br1, A((*e)->s1), x),
     (*e)->s1 = B((*e)->s1),
     x); }

static NoInline bool ana_if_loop(li v, env *e, ob x) { return
  !twop(x) && !(x = (ob) pair(v, nil, nil)) ? false :
  !twop(B(x)) ?
    pushs(v, p_ana_x, A(x), p_ana_if_peek_out, End) :
  pushs(v, BB(x),
           p_ana_if_push_con,
           p_ana_x, AB(x),
           p_ana_if_peek_out,
           End) &&
  ana_if_loop(v, e, *v->sp++) &&
  pushs(v, p_ana_x, A(x), p_ana_if_pop_con, End); }

static mo p_ana_if_pop_out(li v, env *e, size_t m) {
  mo k = pull(v, e, m); return !k ? k :
    ((*e)->s2 = B((*e)->s2), k); }

static NoInline mo ana_if(li v, env *e, size_t m, ob x) {
  return pushs(v, x, p_ana_if_push_out, End) &&
         ana_if_loop(v, e, *v->sp++) &&
         pushs(v, p_ana_if_pop_out, End) ?
         pull(v, e, m) : 0; }

         /*
static ob getnym(env e, ob y) {
  for (ob i = e->s3; twop(i); i = B(i))
    for (ob j = A(i); twop(j); j = B(j))
      if (AA(j) == y) return BA(j);
  return 0; }
  */

enum where { Arg, Loc, Clo, Here, Wait };

static NoInline mo ana_sym_look_top(li v, ob y, mo k) {
  ob x = tbl_get(v, v->lex->topl, y, 0);
  return x ? pullix(imm, x, k) :
    (with(k, x = (ob) pair(v, (ob) v->lex->topl, y)),
     x ? pullix(late, x, k) : 0); }

static NoInline mo ana_sym_look1(li v, env *f, ob y, mo k, env e) {
  size_t n; return
    toplp(e) ? ana_sym_look_top(v, y, k) :
    (lidx(e->loc, y) < 0 &&
     lidx(e->arg, y) < 0 &&
     lidx(e->clo, y) < 0) ?
      ana_sym_look1(v, f, y, k, e->par) :
    (n = llen((*f)->clo),
     with(k, y = (ob) snoc(v, (*f)->clo, y)),
     !y ? 0 : ((*f)->clo = y,
                pullix(clon, putnum(n), k))); }

static NoInline mo ana_sym_look0(li v, env *f, ob y, mo k) {
  ob x; env e = *f; return
    toplp(e) ? ana_sym_look_top(v, y, k) :
    (x = lidx(e->loc, y)) > -1 ? pullix(sl1n, putnum(x), k) :
    (x = lidx(e->arg, y)) > -1 ? pullix(argn, putnum(x), k) :
    (x = lidx(e->clo, y)) > -1 ? pullix(clon, putnum(x), k) :
    ana_sym_look1(v, f, y, k, e->par); }

static NoInline mo ana_sym(li v, env *e, size_t m, ob x) {
  mo k; with(x, k = pull(v, e, m + 2));
  return !k ? k : ana_sym_look0(v, e, x, k); }

static mo p_ana_ap_call(li v, env *e, size_t m) {
  ob ary = *v->sp++;
  mo k = pull(v, e, m + 2);
  return !k ? k :
    G(k) == ret ? pullix(rec, ary, k + 1) :
    pullix(call, ary, k); }

static NoInline mo ana_ap(li v, env *e, size_t m, ob f, ob x) {
  mm(&x);
  bool ok = pushs(v, p_ana_x, f,
                     emi, idmo,
                     p_ana_ap_call, putnum(llen(x)),
                     End);
  for (; ok && twop(x); x = B(x))
    ok = pushs(v, p_ana_x, A(x), emi, push, End);
  return um, ok ? pull(v, e, m) : 0; }

static NoInline bool ana_seq_b_loop(li v, env *e, ob x) {
  bool _; return !twop(x) ||
    (with(x, _ = ana_seq_b_loop(v, e, B(x))),
     _ && pushs(v, p_ana_x, A(x), End)); }

static NoInline mo ana_seq(li v, env *e, size_t m, ob x) {
  return x = twop(x) ? x : (ob) pair(v, x, nil),
         x && ana_seq_b_loop(v, e, x) ? pull(v, e, m) : 0; }

static enum status li_ap(li v, mo f, ob x) {
  mo k = thd(v, immp, x,
                immp, f,
                imm, nil, // assignment target idx=5
                call, putnum(2),
                xok, ap_f, // source idx=9
                End);
  return !k ? OomError :
    (k[5].ap = (vm*) (k + 9), v->ip = k, li_go(v)); }

static NoInline mo ana_mac(li v, env *e, size_t m, ob mac, ob x) {
  ob xp = v->xp; mo ip = v->ip; enum status s; return
    with(xp, with(ip, s = li_ap(v, (mo) mac, x))),
    s != Ok ? (report(v, s), NULL) :
    (x = v->xp, v->xp = xp, v->ip = ip,
     pushs(v, p_ana_x, x, End) ? pull(v, e, m) : 0); }

static NoInline mo ana_two(li v, env *e, size_t m, ob a, ob b) {
  if (symp(a)) {
    if (a == (ob) v->lex->quote) return
      ana_i_x(v, e, m, imm, twop(b) ? A(b) : b);
    if (a == (ob) v->lex->cond)   return ana_if(v, e, m, b);
    if (a == (ob) v->lex->lambda) return ana_fn(v, e, m, b);
    if (a == (ob) v->lex->define) return ana_let(v, e, m, b);
    if (a == (ob) v->lex->begin)  return ana_seq(v, e, m, b);
    ob z = tbl_get(v, v->lex->macros, a, 0);
    if (z) return ana_mac(v, e, m, z, b); }
  return ana_ap(v, e, m, a, b); }

static mo p_alloc(li v, env *e, size_t m) {
  mo k = mo_n(v, m + 1); return !k ? k :
    (setw(k, nil, m), G(k += m) = (vm*) (*e)->name, k); }
