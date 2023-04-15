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
  ob arg, loc, clo, name, asig,
     lams,
     s1, s2, s3;
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
static mo
  p_alloc(li, env*, size_t),
  p_fin(li, env*, size_t),
  ana_if(li, env*, size_t, ob),
  ana_fn(li, env*, size_t, ob),
  ana_let(li, env*, size_t, ob),
  ana_seq(li, env*, size_t, ob),
  ana_mac(li, env*, size_t, ob, ob),
  ana_ap(li, env*, size_t, ob, ob),
  ana_sym(li, env*, size_t, ob),
  ana_two(li, env*, size_t, ob, ob);
static intptr_t
  m_ana_seq(li, env*, ob),
  m_ana_let(li, env*, ob),
  m_ana_fn(li, env*, ob),
  m_ana_if(li, env*, ob),
  m_ana_mac(li, env*, ob, ob),
  m_ana_ap(li, env*, ob),

  n_ana_x(li, env*, ob, intptr_t),
  n_ana_seq(li, env*, ob, intptr_t),
  n_ana_let(li, env*, ob, intptr_t),
  n_ana_fn(li, env*, ob, intptr_t),
  n_ana_if(li, env*, ob, intptr_t),
  n_ana_ip(li, env*, ob, intptr_t),
  n_ana_mac(li, env*, ob, ob, intptr_t),
  n_ana_ap(li, env*, ob, intptr_t);


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

static size_t llenr(ob l, size_t n) {
  return twop(l) ? llenr(B(l), n + 1) : n; }
static size_t llen(ob l) { return llenr(l, 0); }

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

static mo em2k(li v, env *e, mo k) {
  vm *i = (vm*) *v->sp++, *j = (vm*) *v->sp++;
  return k -= 2, G(k) = i, GF(k) = j, k; }
static mo em1k(li v, env *e, mo k) {
  return (--k)->ap = (vm*) *v->sp++, k; }

static mo ana_sym_look0(li, env*, ob, mo);

static mo p_ana_sym(li v, env *e, mo k) {
  ob y = *v->sp++;
  return k ? ana_sym_look0(v, e, y, k) : k; }

static NoInline intptr_t m_ana_imm(li v, env *e, ob x) { return
  pushs(v, em2k, imm, x, End) ? 2 : -OomError; }

static NoInline intptr_t n_ana_imm(li v, env *e, ob x, intptr_t m) { return
  pushs(v, em2k, imm, x, End) ? m + 2 : 0; }

static NoInline intptr_t m_ana_sym(li v, env *e, ob x) { return
  pushs(v, p_ana_sym, x, End) ? 2 : -OomError; }

static NoInline intptr_t n_ana_sym(li v, env *e, ob x, intptr_t m) { return
  pushs(v, p_ana_sym, x, End) ? m + 2 : 0; }

static NoInline intptr_t m_ana_two(li v, env *e, ob x) {
  ob a = A(x), b = B(x);
  if (symp(a)) {
    if (a == (ob) v->lex->quote) return
      m_ana_imm(v, e, twop(b) ? A(b) : b);
    if (a == (ob) v->lex->cond)   return m_ana_if(v, e, b);
    if (a == (ob) v->lex->lambda) return m_ana_fn(v, e, b);
    if (a == (ob) v->lex->define) return m_ana_let(v, e, b);
    if (a == (ob) v->lex->begin)  return m_ana_seq(v, e, b);
    ob z = tbl_get(v, v->lex->macros, a, 0);
    if (z) return m_ana_mac(v, e, z, b); }
  return m_ana_ap(v, e, x); }

static NoInline intptr_t n_ana_two(li v, env *e, ob x, intptr_t m) {
  ob a = A(x), b = B(x);
  if (symp(a)) {
    if (a == (ob) v->lex->quote) return
      n_ana_imm(v, e, twop(b) ? A(b) : b, m);
    if (a == (ob) v->lex->cond)   return n_ana_if(v, e, b, m);
    if (a == (ob) v->lex->lambda) return n_ana_fn(v, e, b, m);
    if (a == (ob) v->lex->define) return n_ana_let(v, e, b, m);
    if (a == (ob) v->lex->begin)  return n_ana_seq(v, e, b, m);
    ob z = tbl_get(v, v->lex->macros, a, 0);
    if (z) return n_ana_mac(v, e, z, b, m); }
  return n_ana_ap(v, e, x, m); }


static intptr_t n_ana_x(li v, env *e, ob x, intptr_t m) { return
  symp(x) ? n_ana_sym(v, e, x, m) :
  twop(x) ? n_ana_two(v, e, x, m) :
            n_ana_imm(v, e, x, m); }

static intptr_t m_ana_x(li v, env *e, ob x) { return
  symp(x) ? m_ana_sym(v, e, x) :
  twop(x) ? m_ana_two(v, e, x) :
            m_ana_imm(v, e, x); }

static mo p_ana_x(li v, env *e, size_t m) {
  ob x = *v->sp++; return
    symp(x) ? ana_sym(v, e, m, x) :
    twop(x) ? ana_two(v, e, m, A(x), B(x)) :
              ana_i_x(v, e, m, imm, x); }

static enum status m_ana(li v, ob x) {
}

static mo ana(li v, ob x) {
  if (!pushs(v, x, emi, ret, p_fin, End)) return 0;
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

  /*
static NoInline mo ana_fn_clo2(li v, env *e, ob x, mo k) {
  ob vars = AB(x);
  intptr_t i = lidx((*e)->loc, A(x));
  mm(&k), mm(&vars);
  for (vars = pushs(v, emi, take2,
                       emi, putnum(llen(vars)),
                       emi, putnum(i),
                       emix, jump, k,
                       p_alloc, End) ? vars : 0;
       vars && twop(vars);
       vars = pushs(v, p_ana_x, A(vars),
                       emi, push, End) ?  B(vars) : 0);
  return
    um, vars = vars ? (ob) pull(v, e, 0) : vars,
    um, !vars ? 0 :
      pullix(jump, vars, k); }
      */

static NoInline mo ana_fn_clo(li v, env *e, ob x, mo k) {
  ob vars = AB(x), code = BB(x);
  mm(&k), mm(&code), mm(&vars);
  for (vars = pushs(v, emix, take, putnum(llen(vars)),
                       p_alloc, End) ? vars : 0;
       vars && twop(vars);
       vars = pushs(v, p_ana_x, A(vars),
                       emi, push, End) ?  B(vars) : 0);
  return
    um, vars = vars ? (ob) pull(v, e, 0) : vars,
    um, vars = vars ? (ob) pair(v, code, vars) : vars,
    um, !vars ? 0 :
      pullix(nilp((*e)->loc) ? encl0 : encl1, vars, k); }

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

static ob ana_fn_inner_inner(li v, env *e, ob x, ob n) {
  intptr_t i = 0;
  ob y = nil;
  with(x, with(n, with(y, x =
    (x = twop(x) ? x : (ob) pair(v, nil, nil)) &&
    (x = linitp(v, x, &y)) &&
    (n = (ob) pair(v, n, (*e)->name)) &&
    pushs(v, p_ana_x, A(y), emi, ret, p_fin, End) &&
    (x = asign(v, x, 0, &i)) &&
    (n = (ob) thd(v, x, nil, nil, // arg loc clo
                     n, putnum(i), // nom asig
                     nil, // lams
                     nil, nil, nil, // s1 s2 s3
                     *e, // par
                     End)) &&
    (x = (ob) pull(v, (env*) &n, 4)) ?
    (
    // x = (ob) iniclocns(v, (env*) &n, (mo) x),
     x = !(i = llen(((env)n)->loc)) ? x :
       (ob) pullix(setloc, putnum(i), (mo) x),
     x = (i = getnum(((env)n)->asig)) > 0 ?
           (ob) pullix(arity, putnum(i), (mo) x) :
         i < 0 ?
           (ob) pullix(varg, putnum(-i-1), (mo) x) :
         x,
     mo_tag((mo) x)->head = (mo) x,
     nilp(((env)n)->clo) ? x :
       (ob) pair(v, ((env)n)->clo, x)) : 0)));
  return x; }

static NoInline mo p_ana_fn_pop(li v, env *e, mo k) {
  ob y = A((*e)->lams);
  (*e)->lams = B((*e)->lams);
  return twop(y) ? ana_fn_clo(v, e, y, k) : pullix(imm, y, k); }

static NoInline intptr_t m_ana_fn(li v, env *e, ob x) {
  ob n = v->sp[0] == (ob) p_ana_let_bind ? v->sp[1] : nil,
     y = (ob) pair(v, x, n);
  if (!y) return -OomError;
  (*e)->lams = y;
  return pushs(v, p_ana_fn_pop, End) ? 5 : -OomError; }

static NoInline intptr_t n_ana_fn(li v, env *e, ob x, intptr_t m) {
  ob n = v->sp[0] == (ob) p_ana_let_bind ? v->sp[1] : nil,
     y = (ob) pair(v, x, n);
  if (!y) return 0;
  (*e)->lams = y;
  return pushs(v, p_ana_fn_pop, End) ? m + 5 : 0; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static NoInline mo ana_fn(li v, env *e, size_t m, ob x) {
  mo k;
  ob y = nil,
     n = v->sp[0] == (ob) p_ana_let_bind ? v->sp[1] : nil;
  with(x, with(n,
    y = (ob) pair(v, x, n),
    y = y ? (ob) pair(v, y, (*e)->lams) : y,
    k = y ? ((*e)->lams = y, pull(v, e, m + 5)) : 0));
  if (!k) return k;
  y = A((*e)->lams), (*e)->lams = B((*e)->lams);
  if (!twop(y)) return pullix(imm, y, k);
  return ana_fn_clo(v, e, y, k); }


static NoInline intptr_t m_ana_let_even(li v, env *e, ob x) {
  if (!twop(x)) return 0;
  if (!(x = rw_let_fn(v, x))) return -OomError;
  intptr_t m; with(x, m = m_ana_let_even(v, e, BB(x)));
  if (m < 0) return m;
  if (!pushs(v, AB(x), p_ana_let_bind, A(x), End)) return -OomError;
  intptr_t n = m_ana_x(v, e, AB(*v->sp++));
  return n < 0 ? n : m + n + 2; }

static NoInline intptr_t n_ana_let_even(li v, env *e, ob x, intptr_t m) {
  if (!twop(x)) return m;
  if (!(x = rw_let_fn(v, x))) return 0;
  with(x, m = n_ana_let_even(v, e, BB(x), m));
  if (!m || !pushs(v, AB(x), p_ana_let_bind, A(x), End)) return 0;
  return n_ana_x(v, e, AB(*v->sp++), m); }

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



static NoInline intptr_t m_ana_let_odd(li v, env *e, ob x) {
  ob _ = nil;
  with(_, x =
    (x = linitp(v, x, &_)) &&
    pushs(v, p_ana_let_right, x, End) ?
      (ob) pair(v, (ob) v->lex->define, v->sp[1]) : 0);
  if (!x || !(x = (ob) pair(v, x, _))
         || !(x = (ob) pair(v, (ob) v->lex->begin, x))
         || !(x = (ob) pair(v, x, nil))
         || !(x = (ob) pair(v, (ob) v->lex->lambda, x))
         || !(x = (ob) pair(v, x, nil)))
    return -OomError;
  intptr_t m = m_ana_x(v, e, x);
  return m < 0 ? m :
    !pushs(v, p_ana_let_left, End) ? -OomError :
    m; }

static NoInline intptr_t n_ana_let_odd(li v, env *e, ob x, intptr_t m) {
  ob _ = nil;
  with(_, x =
    (x = linitp(v, x, &_)) &&
    pushs(v, p_ana_let_right, x, End) ?
      (ob) pair(v, (ob) v->lex->define, v->sp[1]) : 0);
  if (!x || !(x = (ob) pair(v, x, _))
         || !(x = (ob) pair(v, (ob) v->lex->begin, x))
         || !(x = (ob) pair(v, x, nil))
         || !(x = (ob) pair(v, (ob) v->lex->lambda, x))
         || !(x = (ob) pair(v, x, nil)))
    return 0;
  m = n_ana_x(v, e, x, m);
  return m && pushs(v, p_ana_let_left, End) ? m : 0; }

static NoInline intptr_t m_ana_let(li v, env *e, ob x) { return
  !twop(x) ? m_ana_imm(v, e, nil) :
  llen(x) & 1 ? m_ana_let_odd(v, e, x) :
                m_ana_let_even(v, e, x); }

static NoInline intptr_t n_ana_let(li v, env *e, ob x, intptr_t m) { return
  !twop(x) ? n_ana_imm(v, e, nil, m) :
  llen(x) & 1 ? n_ana_let_odd(v, e, x, m) :
                n_ana_let_even(v, e, x, m); }

static NoInline mo ana_let(li v, env *e, size_t m, ob x) {
  bool ok = !twop(x) ? pushs(v, emi, imm, End) :
            llen(x) & 1 ? ana_let_b_odd(v, e, x) :
                          ana_let_b_even(v, e, x);
  return ok ? pull(v, e, m) : 0; }

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

static NoInline intptr_t m_ana_if_loop(li v, env *e, ob x) {
  intptr_t m, n, o;
  if (!twop(x) && !(x = (ob) pair(v, nil, nil))) return -OomError;
  if (!twop(B(x))) return
    !pushs(v, A(x), p_ana_if_peek_out, End) ? -OomError :
    (m = m_ana_x(v, e, *v->sp++)) < 0 ? m : m + 2;
  mm(&x);
  bool ok =
    pushs(v, p_ana_if_peek_out, End) &&
    (m = m_ana_x(v, e, AB(x))) >= 0 &&
    pushs(v, p_ana_if_push_con, End) &&
    (n = m_ana_if_loop(v, e, BB(x))) >= 0 &&
    (o = m_ana_x(v, e, A(x))) >= 0;
  um;
  return ok ? m + n + o + 4 : -OomError; }

static NoInline intptr_t n_ana_if_loop(li v, env *e, ob x, intptr_t m) {
  if (!twop(x) && !(x = (ob) pair(v, nil, nil))) return 0;
  if (!twop(B(x))) return
    !pushs(v, A(x), p_ana_if_peek_out, End) ? 0 :
    (m = n_ana_x(v, e, *v->sp++, m)) ? m + 2 : m;
  mm(&x);
  bool ok =
    pushs(v, p_ana_if_peek_out, End) &&
    (m = n_ana_x(v, e, AB(x), m)) &&
    pushs(v, p_ana_if_push_con, End) &&
    (m = n_ana_if_loop(v, e, BB(x), m)) &&
    (m = n_ana_x(v, e, A(x), m));
  um;
  return m ? m + 4 : 0; }

static mo p_ana_if_pop_out(li v, env *e, size_t m) {
  mo k = pull(v, e, m); return !k ? k :
    ((*e)->s2 = B((*e)->s2), k); }

static NoInline intptr_t m_ana_if(li v, env *e, ob x) {
  if (!pushs(v, x, p_ana_if_push_out, End)) return -OomError;
  intptr_t m = m_ana_if_loop(v, e, *v->sp++);
  if (m >= 0 && !pushs(v, p_ana_if_pop_out, End)) return -OomError;
  return m; }

static NoInline intptr_t n_ana_if(li v, env *e, ob x, intptr_t m) {
  if (!pushs(v, x, p_ana_if_push_out, End)) return 0;
  m = n_ana_if_loop(v, e, *v->sp++, m);
  return m && pushs(v, p_ana_if_pop_out, End) ? m : 0; }

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

static NoInline intptr_t m_ana_ap(li v, env *e, ob x) {
  mm(&x);
  if (!pushs(v, p_ana_ap_call, putnum(llen(x)-1), End)) goto fail;
  intptr_t m, acc = m_ana_x(v, e, A(x));
  if (acc < 0) goto fail;
  for (x = B(x); twop(x); acc += m, x = B(x))
    if (!pushs(v, em1k, push, End) ||
        (m = m_ana_x(v, e, A(x))) < 0) goto fail;
  return um, acc;
fail:
  return um, -OomError; }

static NoInline intptr_t n_ana_ap(li v, env *e, ob x, intptr_t m) {
  if (!pushs(v, x, p_ana_ap_call, putnum(llen(x)-1), End)) return 0;
  x = *v->sp++, mm(&x);
  for (m = n_ana_x(v, e, A(x), m), x = B(x); m && twop(x); x = B(x))
    m = !pushs(v, em1k, push, End) ? 0 : n_ana_x(v, e, A(x), m);
  return um, m; }

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

static intptr_t m_ana_seq_loop(li, env*, ob);
static NoInline intptr_t m_ana_seq(li v, env *e, ob x) {
  x = twop(x) ? x : (ob) pair(v, x, nil);
  return x ? m_ana_seq_loop(v, e, x) : -OomError; }

static NoInline intptr_t m_ana_seq_loop(li v, env *e, ob x) {
  if (!twop(x)) return 0;
  intptr_t m, n;
  with(x, m = m_ana_seq_loop(v, e, B(x)));
  if (m < 0) return m;
  n = m_ana_x(v, e, A(x));
  return n < 0 ? n : m + n; }

static intptr_t n_ana_seq_loop(li, env*, ob, intptr_t);
static NoInline intptr_t n_ana_seq_loop(li v, env *e, ob x, intptr_t m) {
  if (!twop(x)) return m;
  with(x, m = n_ana_seq_loop(v, e, B(x), m));
  return !m ? m : n_ana_x(v, e, A(x), m); }

static NoInline intptr_t n_ana_seq(li v, env *e, ob x, intptr_t m) { return
  x = twop(x) ? x : (ob) pair(v, x, nil),
  x ? n_ana_seq_loop(v, e, x, m) : 0; }

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

static NoInline intptr_t m_ana_mac(li v, env *e, ob mac, ob x) {
  ob xp = v->xp; mo ip = v->ip; enum status s;
  with(xp, with(ip, s = li_ap(v, (mo) mac, x)));
  if (s != Ok) return -s;
  x = v->xp, v->xp = xp, v->ip = ip;
  return m_ana_x(v, e, x); }

static NoInline intptr_t n_ana_mac(li v, env *e, ob mac, ob x, intptr_t m) {
  ob xp = v->xp; mo ip = v->ip; enum status s;
  with(xp, with(ip, s = li_ap(v, (mo) mac, x)));
  if (s != Ok) return 0; // FIXME forgets error
  x = v->xp, v->xp = xp, v->ip = ip;
  return n_ana_x(v, e, x, m); }

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


static NoInline mo p_alloc(li v, env *e, size_t m) {
  mo k = mo_n(v, m + 1);
  if (k) setw(k, nil, m), G(k += m) = (vm*) (*e)->name;
  return k; }

static mo p_fin(li v, env *e, size_t m) {
  ob q, y, lams = (*e)->lams;
  mm(&lams);
  for (; twop(lams); A(lams) = y, lams = B(lams)) {
    y = ana_fn_inner_inner(v, e, AA(lams), BA(lams));
    if (y && twop(y))
      with(y,
        q = (ob) nym(v),
        q = q ? (ob) pair(v, q, (*e)->loc) : q,
        q = q ? A((*e)->loc = q) : q),
      y = q ? (ob) pair(v, q, y) : q;
    if (!y) return um, NULL; }
  um;
  return p_alloc(v, e, m); }
