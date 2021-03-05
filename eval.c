#include "lips.h"
#include "terp.h"
////
/// bootstrap thread compiler
//
// it's basically an "analyzing evaluator" that produces
// threads that run on the lisp vm. each code-generating
// function first generates its immediate continuation,
// so there's an implicit CPS transformation. compiler
// functions explicitly construct their own call sequence
// at run time by using the main stack to store code and
// pointers to compiler entry points, which are then
// accessed like this:
#define ccc ((c1*)Gn(*Sp++))
//
// the compiler does a few optimizations which we can
// safely remove once the self-hosted compiler works.
typedef obj c1(vm, mem, num),
            c2(vm, mem, num, obj),
            c3(vm, mem, num, obj, obj);
static void c_de_r(vm, mem, obj),
            scan(vm, mem, obj), pushs(vm, ...);
static Inline c2 *inliner(vm, mem, obj);
static c1  c_ev, produce, c_d_bind, inst, insx, c_ini;
static c2 c_eval, c_sy, c_2, c_imm, ltu, c_ap;
static c3 c_la_clo;
static obj hom_ini(vm, num), tupl(vm, ...), hom_fin(vm, obj),
           def_sug(vm, obj), snoc(vm, obj, obj),
           look(vm, obj, obj);
static num lidx(obj, obj);
static obj linitp(vm, obj, mem);
static obj imx(vm, mem, num, terp*, obj);
enum location { Here, Loc, Arg, Clo, Wait };

#define N(x) putnum(x)
#define Gn(x) getnum(x)
#define End 0l
#define Push(...) pushs(v,__VA_ARGS__,End)
// compile current continuation
// it uses the main data stack as an alternate return stack
// on which it can explicitly construct its own continuations.
// any time you see something like
//    return Push(...), ccc(v, e, m)
// that's basically a hack to enable CPS in C.

// the compiler uses three stacks:
// 0. stack 0 is the main data/call stack, and
//    is used for compiler continuations
// 1. stack 1 is implemented with pairs in Xp,
//    and is used for conditional branch entry
//    points
// 2. stack 2 is implemented with pairs in Ip,
//    and is used for conditional exit points

// " compilation environments "
// the compilation environment is represented as a pointer to
// a tuple with a structure specified below, or nil for the
// global environment. the object should be gc protected.
#define arg(x)  AR(x)[0]
#define loc(x)  AR(x)[1]
#define clo(x)  AR(x)[2]
#define par(x)  AR(x)[3]
#define lams(x) AR(x)[4]
#define name(x) AR(x)[5]
#define vals(x) AR(x)[6]
#define asig(x) AR(x)[7]
// 1. the list of arguments to the procedure being compiled
// 2. the list of local variables defined in the procedure
// 3. the list of closure variables imported into the procedure
// 4. the parent (lexical) scope of the procedure
// 5. the dictionary of known variable values for this procedure

#define Arity(n) {\
  num i = llen(Y(x));\
  if (i<n) return arity_error(v, e, x, i, n); }

static obj arity_error(vm v, mem e, obj x, num h, num w) {
  return err(v, "compile", x, "wrong arity : %ld of %ld", h, w); }
static obj type_error(vm v, mem e, obj x, enum type h, enum type w) {
  return err(v, "compile", x, "wrong type : %s for %s", tnom(h), tnom(w)); }
#define toplp(x) nilp(*x)
#define c1(nom,...) static obj nom(vm v,mem e,num m,##__VA_ARGS__)
#define c2(nom,...) static obj nom(vm v,mem e,num m,obj x,##__VA_ARGS__)

static Inline obj em1(terp *i, obj k) {
  hom h = gethom(k)-1;
  G(h) = i;
  return puthom(h); }

static Inline obj em2(terp *i, obj j, obj k) {
  return em1(i, em1((terp*)j, k)); }

#define None 8
static enum type consumes(obj h) {
  terp *i = G(h);
  return i == tchom ? Hom : i == tcnum ? Num : i == tctwo ? Two : None; }

// totally dumb procedural compile time type checking:
// check if your immediate continuation is a
// type check. if so and it conflicts then it errors at
// compile time. but if it's compatible then the check
// gets eliminated.
c1(produce) {
  enum type u, t = Gn(*Sp++);
  obj k, x = *Sp++;
  with(x, k = ccc(v, e, m));
  while (t == (u = consumes(k))) k += Word;
  return u == None ? k : type_error(v, e, x, t, u); }

static obj compile(vm v, obj x) {
  static obj top = nil;
  Push(N(c_ev), x, N(inst), N(yield), N(c_ini));
  return ccc(v, &top, 0); }

/// evaluate an expression
obj eval(vm v, obj x) {
  hom h = gethom(compile(v, x));
  return G(h)(v, h, v->fp, v->sp, v->hp, nil); }

static void scan_def_add(vm v, mem e, obj y, obj x) {
  mm(&x);
  switch (kind(x)) {
    case Two:
      if (X(x) == Qt) { x = twop(Y(x)) ? XY(x) : x; goto imm; }
      if (X(x) == La) with(y, tbl_set(v, lams(*e), y, x));
    case Sym: y = pair(v, y, loc(*e)), loc(*e) = y;  break;
    default: imm: tbl_set(v, vals(*e), y, x); }
  um, scan(v, e, x); }

static int scan_def(vm v, mem e, obj x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(Y(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  obj r; with(x, r = scan_def(v, e, YY(x)));
  if (r) scan_def_add(v, e, X(x), XY(x));
  return r; }

static void scan(vm v, mem e, obj x) {
  if (!twop(x) || X(x) == La || X(x) == Qt) return;
  if (X(x) == De) scan_def(v, e, Y(x));
  else {
    for (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
    um; } }

static obj asign(vm v, obj a, num i, mem m) {
  if (!twop(a)) return *m = i, a;
  if (twop(Y(a)) && XY(a) == Va)
    return *m = -(i+1), pair(v, X(a), nil);
  obj x;
  with(a, x = asign(v, Y(a), i+1, m));
  return pair(v, X(a), x); }

static obj scope(vm v, mem e, obj a, obj n) {
  num s = 0;
  with(n, a = asign(v, a, 0, &s));
  obj x, y = tupl(v, a, nil, nil, *e, nil, n, nil, N(s), End);
  with(y, x = table(v), vals(y) = x,
          x = table(v), lams(y) = x);
  return y; }

static void precompile_inner_lambdas(vm v, mem e) {
  obj ks = tbl_keys(v, lams(*e));
  for (mm(&ks); twop(ks); ks = Y(ks)) {
    obj y = tbl_get(v, lams(*e), X(ks));
    if (homp(y = ltu(v, e, X(ks), y)))
      tbl_set(v, vals(*e), X(ks), y),
      tbl_del(v, lams(*e), X(ks)),
      y = ldel(v, loc(*e), X(ks)),
      loc(*e) = y;
    else
      Y(y) = tbl_get(v, lams(*e), X(ks)),
      tbl_set(v, lams(*e), X(ks), y); }
  um; }

static int okvs(vm v, mem e, obj vs) {
  if (!twop(vs)) return 1;
  if (!tbl_get(v, vals(*e), X(vs)) &&
      !tbl_get(v, lams(*e), X(vs)))
    return 0;
  return okvs(v, e, Y(vs)); }

static void eliminate_runtime_dependencies(vm v, mem e) {
  for (obj ks = tbl_keys(v, lams(*e)); twop(ks); ks = Y(ks)) {
    obj vs = X(tbl_get(v, lams(*e), X(ks)));
    if (!okvs(v, e, vs)) {
      tbl_del(v, lams(*e), X(ks));
      return eliminate_runtime_dependencies(v, e); } } }

static void recompile_inner_lambdas(vm v, mem e, mem d, obj ks) {
  obj x;
  if (twop(ks))
    with(ks, x = ldel(v, loc(*e), X(ks)), loc(*e) = x,
             recompile_inner_lambdas(v, e, d, Y(ks)),
             x = ltu(v, e, X(ks), Y(tbl_get(v, *d, X(ks))))),
    tbl_set(v, vals(*e), X(ks), x); }

static void resolve(vm v, mem e) {
  precompile_inner_lambdas(v, e);
  eliminate_runtime_dependencies(v, e);
  obj ks = tbl_keys(v, lams(*e)), ls = lams(*e);
  with(ls, recompile_inner_lambdas(v, e, &ls, lams(*e) = ks)); }

static obj compose(vm v, mem e, obj x) {
  Push(N(c_ev), x, N(inst), N(ret), N(c_ini));
  scan(v, e, Sp[1]);
  resolve(v, e);
  obj i; x = ccc(v, e, 4); // 4 = 2 + 2
  if ((i = llen(loc(*e)))) x = em2(prel,  N(i), x);
  i = Gn(asig(*e));
  if (i > 0) x = em2(arity, N(i), x);
  else if (i < 0) x = em2(vararg, N(-i-1), x);
  x = hom_fin(v, x);
  return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static obj ltu(vm v, mem e, obj n, obj l) {
  obj y;
  l = Y(l);
  with(n,
    l = twop(l) ? l : pair(v, l, nil),
    with(y, l = linitp(v, l, &y),
            n = scope(v, e, l, n)),
    l = compose(v, &n, X(y)));
  return l; }

c1(c_ev) { return c_eval(v, e, m, *Sp++); }
c2(c_eval) {
  return (symp(x) ? c_sy : twop(x) ? c_2 : c_imm)(v, e, m, x); }

c2(c_la) {
  terp *j = immv;
  obj k;
  obj nom = *Sp == N(c_d_bind) ? Sp[1] : nil;
  with(nom, with(x,
    Push(N(produce), N(Hom), x),
    k = ccc(v, e, m+2)));
  with(k,
    x = homp(x = ltu(v, e, nom, x)) ? x :
    (j = toplp(e) || !twop(loc(*e)) ? encln : encll,
     c_la_clo(v, e, m, X(x), Y(x))));
  return em2(j, x, k); }

c2(c_imm) {
  return Push(N(immv), x, N(produce), N(kind(x)), x),
         insx(v, e, m); }

static obj c_la_clo(vm v, mem e, num m, obj arg, obj seq) {
  num i = llen(arg);
  mm(&arg), mm(&seq);
  for (Push(N(insx), N(take), N(i), N(c_ini));
       twop(arg);
       Push(N(c_ev), X(arg), N(inst), N(push)), arg = Y(arg));
  return arg = ccc(v, e, 0), um, um, pair(v, seq, arg); }

c1(c_d_bind) {
  obj y = *Sp++;
  return toplp(e) ?
    imx(v, e, m, tbind, y) :
    imx(v, e, m, setl, N(lidx(loc(*e), y))); }

static c3 late;
c1(c_ev_d) {
  obj w = *Sp++, y;
  mm(&w);
  if (toplp(e) || lidx(loc(*e), X(w)) != -1)
    Push(N(c_d_bind), X(w));
  y = look(v, *e, X(w));
  return um,
    X(y) == N(Here) ? c_imm(v, e, m, Y(y)) :
    X(y) == N(Wait) && Y(y) != Dict ?
      late(v, e, m, X(w), Y(y)) :
    c_eval(v, e, m, XY(w)); }

static void c_de_r(vm v, mem e, obj x) {
  if (twop(x))
    with(x, c_de_r(v, e, YY(x))),
    Push(N(c_ev_d), x); }

c2(c_de) {
  return !twop(Y(x)) ? c_imm(v, e, m, nil) :
         llen(Y(x)) % 2 ?  c_eval(v, e, m, def_sug(v, x)) :
         (c_de_r(v, e, Y(x)), ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 Xp
#define S2 Ip
//
// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
  obj x = ccc(v, e, m);
  x = pair(v, x, S2);
  return X(S2 = x); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
  obj x = ccc(v, e, m+2), k = X(S2);
  terp *i = G(k);
  return
    i == ret ? em1(i, x) :
    em2(jump, i == jump ? (obj) GF(k) : k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
  obj x = ccc(v, e, m);
  x = pair(v, x, S1);
  return X(S1 = x); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
  obj x = ccc(v, e, m+2);
  return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static void c_co_r(vm v, mem e, obj x) {
  if (!twop(x)) x = pair(v, nil, nil);
  if (!twop(Y(x)))
    Push(N(c_ev), X(x), N(c_co_pre_con));
  else
    with(x,
      Push(N(c_co_post_con), N(c_ev), XY(x), N(c_co_pre_con)),
      c_co_r(v, e, YY(x))),
    Push(N(c_ev), X(x), N(c_co_pre_ant)); }

c2(c_co) {
  return with(x, Push(N(c_co_pre))),
         c_co_r(v, e, Y(x)),
         x = ccc(v, e, m), S2 = Y(S2), x; }

static void c_se_r(vm v, mem e, obj x) {
  if (twop(x)) with(x, c_se_r(v, e, Y(x))),
               Push(N(c_ev), X(x)); }
c2(c_se) {
  if (!twop(x = Y(x))) x = pair(v, nil, nil);
  return c_se_r(v, e, x), ccc(v, e, m); }

// this function emits an instruction to call a
// stored at runtime in xp.
c1(c_call) {
  obj a = *Sp++, k = ccc(v, e, m+2);
  return G(k) != ret ? em2(call, a, k) :
         Gn(a) == llen(arg(*e)) ? em1(loop, k) :
         em2(recur, a, k); }

static obj topl_lookup(vm v, obj y) {
  obj q = tbl_get(v, Dict, y);
  return q ? q : tbl_get(v, v->cdict, y); }


#define L(n,x) pair(v, N(n), x)
static obj look(vm v, obj e, obj y) {
  obj q;
  if (nilp(e)) return (q = topl_lookup(v, y)) ?
    L(Here, q) : L(Wait, Dict);
  if ((q = tbl_get(v, vals(e), y))) return L(Here, q);
  if ((q = lidx(lams(e), y)) != -1) return L(Wait, vals(e));
  if ((q = lidx(loc(e), y)) != -1) return L(Loc, e);
  if ((q = lidx(arg(e), y)) != -1) return L(Arg, e);
  if ((q = lidx(clo(e), y)) != -1) return L(Clo, e);
  return look(v, par(e), y); }
#undef L

static obj imx(vm v, mem e, num m, terp *i, obj x) {
  return Push(N(i), x), insx(v, e, m); }

c2(late, obj d) {
  obj k;
  x = pair(v, d, x);
  with(x, k = ccc(v, e, m+2));
  enum type t = consumes(k);
  if (t != None) k += Word;
  with(k, x = pair(v, N(t), x));
  return em2(lbind, x, k); }

c2(c_sy) {
  obj y, q;
  if (toplp(e)) return (q = topl_lookup(v, x)) ?
    c_imm(v, e, m, q) : late(v, e, m, x, Dict);
  if ((q = tbl_get(v, vals(*e), x))) return c_imm(v, e, m, q);
  if ((q = lidx(lams(*e), x)) != -1) return late(v, e, m, x, vals(*e));
  if ((q = lidx(loc(*e), x)) != -1) return imx(v, e, m, locn, N(q));
  if ((q = lidx(arg(*e), x)) != -1) return imx(v, e, m, argn, N(q));
  if ((q = lidx(clo(*e), x)) != -1) return imx(v, e, m, clon, N(q));

  // the symbol isn't bound locally so search the enclosing scopes
  with(x, q = look(v, par(*e), x));
  switch (Gn(X(q))) {
    case Here: return c_imm(v, e, m, Y(q));
    case Wait: return late(v, e, m, x, Y(q));
    default:
      y = llen(clo(*e));
      with(x, q = snoc(v, clo(*e), x));
      clo(*e) = q;
      return imx(v, e, m, clon, N(y)); } }

c2(c_qt) { return c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }

c2(c_2) {
  obj z = X(x);
  return
    z == Qt ? c_qt(v, e, m, x) :
    z == If ? c_co(v, e, m, x) :
    z == De ? c_de(v, e, m, x) :
    z == La ? c_la(v, e, m, x) :
    z == Se ? c_se(v, e, m, x) :
    c_ap(v, e, m, x); }

c2(c_ap) {
  c2 *c;
  with(x, c = inliner(v, e, X(x)));
  if (c) return c(v, e, m, x);
  for (mm(&x),
       Push(N(c_ev), X(x), N(inst), N(tchom),
            N(c_call), N(llen(Y(x))));
       twop(x = Y(x));
       Push(N(c_ev), X(x), N(inst), N(push)));
  return um, ccc(v, e, m); }

c1(inst) {
  terp *i = (terp*) Gn(*Sp++);
  return em1(i, ccc(v, e, m+1)); }

c1(insx) {
  terp *i = (terp*) Gn(*Sp++);
  obj x = *Sp++, k;
  with(x, k = ccc(v, e, m+2));
  return em2(i, x, k); }

c1(c_ini) {
  obj k = hom_ini(v, m+1);
  if (!toplp(e)) k = em1((terp*)name(*e), k);
  return k; }

static obj c_un_t(vm v, mem e, num m, obj x, terp *o, terp *tc) {
  Arity(1);
  Push(XY(x), N(inst), N(tc), N(inst), N(o));
  return c_ev(v, e, m); }

static obj c_bin_tx(vm v, mem e, num m, obj x, terp *o, terp *tc) {
  Arity(2);
  Push(X(YY(x)), N(inst), N(push), N(c_ev), XY(x),
       N(inst), N(tc), N(inst), N(o));
  return c_ev(v, e, m); }

static void c_m_bin_r(vm v, mem e, obj x, terp *o) {
  if (twop(x))
    with(x, c_m_bin_r(v, e, Y(x), o)),
    Push(N(inst), N(push),
         N(c_ev), X(x),
         N(inst), N(tcnum),
         N(inst), N(o)); }

c2(c_m_bin_lr, terp *i, terp *j, num z) {
  if (!twop(Y(x))) return c_imm(v, e, m, N(z));
  Push(N(produce), N(Num), x);
  x = Y(Sp[2]);
  if (!twop(Y(x))) { if (j) with(x, Push(N(inst), N(j))); }
  else with(x, c_m_bin_r(v, e, Y(x), i));
  return Push(X(x), N(inst), N(tcnum)), c_ev(v, e, m); }

static obj c_bin(vm v, mem e, num m, obj x, terp *o) {
  Push(X(x), N(inst), N(push), N(c_ev), XY(x), N(inst), N(o));
  return c_ev(v, e, m); }
c2(cons_c) {
  Arity(2);
  Push(XY(x), N(inst), N(push), N(c_ev), X(YY(x)), N(inst), N(cons), N(produce), N(Two), x);
  return c_ev(v, e, m); }

c2(fail_c) {
  Push(twop(Y(x)) ? XY(x) : nil, N(inst), N(fail));
  return c_ev(v, e, m); }

#define apc(f,...) (f)(v,e,m,##__VA_ARGS__)
c2(c_add) { return apc(c_m_bin_lr, x, add, NULL, 0); }
c2(c_sub) { return apc(c_m_bin_lr, x, sub, neg, 0); }
c2(mul_c) { return apc(c_m_bin_lr, x, mul, NULL, 1); }
c2(div_c) { return apc(c_m_bin_lr, x, dqv, NULL, 1); }
c2(mod_c) { return apc(c_m_bin_lr, x, mod, NULL, 1); }
c2(car_c) { return apc(c_un_t, x, car, tctwo); }
c2(cdr_c) { return apc(c_un_t, x, cdr, tctwo); }
c2(rpla_c) { return apc(c_bin_tx, x, setcar, tctwo); }
c2(rpld_c) { return apc(c_bin_tx, x, setcdr, tctwo); }

c1(or_c_pre_op) {
  obj x = ccc(v, e, m+2), k = X(S2);
  return em2(branch, k, x); }

c1(and_c_pre_op) {
  obj x = ccc(v, e, m+2), k = X(S2);
  return em2(barnch, k, x); }

c1(cnop) { return ccc(v, e, m); }

static void c_cmp_r(vm v, mem e, obj x, terp *o) {
  if (!twop(x)) return Push(N(c_co_pre), N(inst), N(drop));
  with(x, c_cmp_r(v, e, Y(x), o));
  Push(N(c_ev), X(x), N(inst), N(tuck), N(inst), N(o),
       N(and_c_pre_op)); }

static void c_pr_r(vm v, mem e, obj x, terp *p) {
  if (!twop(x)) return Push(N(c_co_pre));
  with(x, c_pr_r(v, e, Y(x), p));
  Push(N(c_ev), X(x), N(inst), N(p), N(and_c_pre_op)); }

c2(c_pr, terp *p) {
  if (!twop(x)) return c_imm(v, e, x, nil);
  if (!twop(Y(x))) return
    Push(X(x), N(inst), N(p)),
    c_ev(v, e, m);
  return
    c_pr_r(v, e, x, p),
    x = ccc(v, e, m),
    S2 = Y(S2),
    x; }

c2(nump_c) { return c_pr(v, e, m, Y(x), numpp); }
c2(homp_c) { return c_pr(v, e, m, Y(x), hompp); }
c2(strp_c) { return c_pr(v, e, m, Y(x), strpp); }
c2(nilp_c) { return c_pr(v, e, m, Y(x), nilpp); }
c2(symp_c) { return c_pr(v, e, m, Y(x), sympp); }
c2(tblp_c) { return c_pr(v, e, m, Y(x), tblpp); }
c2(twop_c) { return c_pr(v, e, m, Y(x), twopp); }

c2(c_cmp, terp *j) {
  if (!twop(x)) return c_imm(v, e, x, nil);
  if (!twop(Y(x))) return c_imm(v, e, m, N(0));
  if (!twop(YY(x))) return c_bin(v, e, m, x, j);
  with(x, c_cmp_r(v, e, Y(x), j));
  Push(X(x), N(inst), N(push));
  return x = c_ev(v, e, m), S2 = Y(S2), x; }

static void cunvr(vm v, mem e, obj x, obj xs, c1 *sep, c1 *fin) {
  if (nilp(xs)) return Push(N(c_ev), x, N(fin ? fin : cnop));
  with(x, cunvr(v, e, X(xs), Y(xs), sep, fin));
  Push(N(c_ev), x, N(sep ? sep : cnop)); }

static obj cunv(vm v, mem e, num m, obj x, c1*sep, c1*fin) {
  if (!twop(x)) return c_imm(v, e, m, nil);
  cunvr(v, e, X(x), Y(x), sep, fin);
  return ccc(v, e, m); }

c2(lt_c) { return c_cmp(v, e, m, Y(x), lt); }
c2(lteq_c) { return c_cmp(v, e, m, Y(x), lteq); }
c2(eq_c) { return c_cmp(v, e, m, Y(x), eq); }
c2(gteq_c) { return c_cmp(v, e, m, Y(x), gteq); }
c2(gt_c) { return c_cmp(v, e, m, Y(x), gt); }
c2(or_c) { return x = cunv(v, e, m, Y(x), or_c_pre_op, c_co_pre), S2 = Y(S2), x; }
c2(and_c) { return x = cunv(v, e, m, Y(x), and_c_pre_op, c_co_pre), S2 = Y(S2), x; }
c1(emsepsp) { return imx(v, e, m, emse, putnum(' ')); }
c1(emsepnl) { return imx(v, e, m, emse, putnum('\n')); }
c2(em_c) { return cunv(v, e, m, Y(x), emsepsp, emsepnl); }

struct intro_rec {
  const char *nom;
  terp *addr; };


static void rpr(vm v, mem d, const char *n, terp *u, c2 *c) {
  obj x, y = interns(v, n);
  with(y, x = hom_ini(v, 2));
  x = em2(u, y, x);
  tbl_set(v, *d, y, x);
  if (c) tbl_set(v, *d, N(u), N(c)); }
static void rin(vm v, mem d, const char *n, terp *u) {
  obj y = interns(v, n);
  tbl_set(v, *d, y, putnum(u)); }

#define prims(_)\
  _(".", em_u, em_c),\
  _("*:", car_u, car_c),     _(":*", cdr_u, cdr_c),\
  _("*!", setcar_u, rpla_c), _("!*", setcdr_u, rpld_c),\
  _("::", cons_u, cons_c),   _("=", eq_u, eq_c),\
  _("<", lt_u, lt_c),        _("<=", lteq_u, lteq_c),\
  _(">", gt_u, gt_c),        _(">=", gteq_u, gteq_c),\
  _("+", add_u, c_add),      _("-", sub_u, c_sub),\
  _("*", mul_u, mul_c),      _("/", div_u, div_c),\
  _("%", mod_u, mod_c),      _("ap", ap_u, NULL),\
  _("ccc", ccc_u, NULL),     _("ev", ev_u, NULL),\
  _("||", or_u, or_c),       _("&&", and_u, and_c),\
  _("fail", fail_u, fail_c), _("tbl", tblmk, NULL),\
  _("tbl-get", tblg, NULL),  _("tbl-set", tbls, NULL),\
  _("tbl-has", tblc, NULL),  _("tbl-del", tbld, NULL),\
  _("tbl-keys", tblks, NULL),_("tbl-len", tbll, NULL),\
  _("str-len", strl, NULL),  _("str-get", strg, NULL),\
  _("str", strmk, NULL),     _(".c", pc_u, NULL),\
  _("hom", hom_u, NULL),     _("hom-seek", hom_seek_u, NULL),\
  _("hom-set-x", hom_setx_u, NULL), _("hom-get-x", hom_getx_u, NULL),\
  _("hom-set-i", hom_seti_u, NULL), _("hom-get-i", hom_geti_u, NULL),\
  _("zzz", zzz, NULL),       _("nump", nump_u, nump_c),\
  _("symp", symp_u, symp_c), _("twop", twop_u, twop_c),\
  _("tblp", tblp_u, tblp_c), _("strp", strp_u, strp_c),\
  _("nilp", nilp_u, nilp_c), _("homp", homp_u, homp_c)

#define RPR(a,b,c) rpr(v,&d,a,b,c)
#define RIN(x) rin(v,&d,"i-"#x,x)
static Inline obj code_dictionary(vm v) {
  obj d = table(v);
  with(d, prims(RPR), insts(RIN));
  return d; }
#undef RPR
#undef RIN

#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
static Inline obj syntax_array(vm v) {
  tup t = cells(v, Size(tup) + NSyns);
  t->len = NSyns, memset(t->xs, -1, w2b(NSyns));
  obj z, y = puttup(t);
  with(y,
    bsym(Def, ":"), bsym(Cond, "?"), bsym(Lamb, "\\"),
    bsym(Quote, "`"), bsym(Seq, ","), bsym(Splat, "."));
#undef bsym
  return y; }



vm initialize() {
  vm v = malloc(sizeof(struct rt));
  if (!v) errp(v, "init", 0, "malloc failed");
  else if (setjmp(v->restart))
    errp(v, "init", 0, "failed"), finalize(v), v = NULL;
  else {
    v->t0 = clock();
    v->ip = v->xp = v->dict = v->syms = v->syn = v->cdict = nil;
    v->fp = v->hp = v->sp = (mem)w2b(1);
    v->count = 0, v->mem_len = 1, v->mem_pool = NULL;
    v->mem_root = NULL;
    v->syn = syntax_array(v);
    v->cdict = code_dictionary(v);
    v->dict = table(v); }
  return v; }

void finalize(vm v) {
  free(v->mem_pool), free(v); }

static Inline c2 *inliner(vm v, mem e, obj x) {
  if (symp(x)) x = X(x = look(v, *e, x)) == N(Here) ? Y(x) : 0;
  if (x && homp(x) && (x = tbl_get(v, v->cdict, N(G(x)))))
    return (c2*) Gn(x);
  return NULL; }

static obj snoc(vm v, obj l, obj x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = snoc(v, Y(l), x));
  return pair(v, X(l), x); }

static obj linitp(vm v, obj x, mem d) {
  if (!twop(Y(x))) return *d = x, nil;
  obj y; with(x, y = linitp(v, Y(x), d));
  return pair(v, X(x), y); }

// syntactic sugar for define
static obj def_sug(vm v, obj x) {
  obj y = nil;
  with(y, x = linitp(v, x, &y));
  x = pair(v, x, y),   x = pair(v, Se, x);
  x = pair(v, x, nil), x = pair(v, La, x);
  return pair(v, x, nil); }

// list functions
static num lidx(obj l, obj x) {
  num i = 0;
  for (; twop(l); l = Y(l), i++) if (x == X(l)) return i;
  return -1; }

num llen(obj l) {
  for (num i = 0;; l = Y(l), i++) if (!twop(l)) return i; }

static tup tuplr(vm v, num i, va_list xs) {
  tup t; obj x = va_arg(xs, obj);
  return x ?
    (with(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
    ((t = cells(v, Size(tup) + i))->len = i, t); }

static obj tupl(vm v, ...) {
  tup t; va_list xs;
  va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs);
  return puttup(t); }

static void pushss(vm v, num i, va_list xs) {
  obj x = va_arg(xs, obj);
  if (x) with(x, pushss(v, i+1, xs)), *--Sp = x;
  else if (Avail < i) reqsp(v, i); }

static void pushs(vm v, ...) {
  va_list xs; va_start(xs, v), pushss(v, 0, xs), va_end(xs); }

static obj hom_ini(vm v, num n) {
  hom a = cells(v, n + 2);
  return G(a+n) = NULL,
         GF(a+n) = (terp*) a,
         memset(a, -1, w2b(n)),
         puthom(a+n); }

static obj hom_fin(vm v, obj a) {
  for (hom b = gethom(a);;) if (!b++->g)
    return (obj) (b->g = (terp*) a); }

obj homnom(vm v, obj x) {
  terp *k = G(x);
  if (k == clos || k == pc0 || k == pc1)
    x = (obj) G(FF(x));
  mem h = (mem) gethom(x);
  while (*h) h++;
  x = h[-1];
  return (mem)x >= Pool &&
         (mem)x < Pool+Len &&
         symp(x) ? x : nil; }
