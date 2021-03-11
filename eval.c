#include "lips.h"
#include "terp.h"
#include "eval.h"
////
/// bootstrap thread compiler
//
static void c_de_r(vm, mem, obj),
            scan(vm, mem, obj), pushs(vm, ...);
static c1  c_ev, c_d_bind, inst, insx, c_ini;
static c2 c_eval, c_sy, c_2, c_imm, ltu, c_ap, c_la_clo;
static c3 late;
static obj tupl(vm, ...),
           def_sug(vm, obj), snoc(vm, obj, obj),
           look(vm, obj, obj);
static num idx(obj, obj);
static obj linitp(vm, obj, mem);
static obj imx(vm, mem, num, terp*, obj);
enum location { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) static obj nom(vm v,mem e,num m,##__VA_ARGS__)
#define c2(nom,...) static obj nom(vm v,mem e,num m,obj x,##__VA_ARGS__)


// it's basically an "analyzing evaluator" that produces
// threads that run on the lisp vm. each code-generating
// function first generates its immediate continuation,
// so there's an implicit CPS transformation. continuations
// are explicitly constructed by pushing function pointers
// onto the main lips stack like this:
#define Push(...) pushs(v,__VA_ARGS__,non)
// and then popped off like this:
#define ccc ((c1*)Gn(*Sp++))
// there's a natural correspondence between the Push(...),
// ccc(...) pattern used in this file and normal continuation
// passing style in lisp (cf. the stage 2 compiler).

// in addition to the main stack, the compiler uses Xp and Ip
// as stacks for storing code entry points when generating
// conditionals, which is admittedly kind of sus.

// " compilation environments "
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define toplp(x) nilp(*x)
#define arg(x)  AR(x)[0] // argument variables : a list
#define loc(x)  AR(x)[1] // local variables : a list
#define clo(x)  AR(x)[2] // closure variables : a list
#define par(x)  AR(x)[3] // surrounding scope : tuple or nil
#define name(x) AR(x)[4] // function name : a symbol or nil
#define asig(x) AR(x)[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.

static obj compile(vm v, obj x) {
  static obj top = nil;
  Push(N(c_ev), x, N(inst), N(yield), N(c_ini));
  return ccc(v, &top, 0); }

/// evaluate an expression
obj eval(vm v, obj x) {
  hom h = gethom(compile(v, x));
  return G(h)(v, h, v->fp, v->sp, v->hp, nil); }

static void scan_def_add(vm v, mem e, obj y, obj x) {
  with(x, y = pair(v, y, loc(*e)), loc(*e) = y);
  scan(v, e, x); }

static int scan_def(vm v, mem e, obj x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(Y(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  obj r; with(x, r = scan_def(v, e, YY(x)));
  if (r) scan_def_add(v, e, X(x), XY(x));
  return r; }

static void scan(vm v, mem e, obj x) {
  if (!twop(x) || X(x) == La || X(x) == Qt) return;
  if (X(x) == De) return (void) scan_def(v, e, Y(x));
  for (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
  um; } 

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
  return tupl(v, a, nil, nil, *e, n, N(s), non); }

static obj compose(vm v, mem e, obj x) {
  Push(N(c_ev), x, N(inst), N(ret), N(c_ini));
  scan(v, e, Sp[1]);
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
c2(c_eval) { return (symp(x) ? c_sy : twop(x) ? c_2 : c_imm)(v, e, m, x); }

c2(c_la) {
  terp *j = immv;
  obj k;
  obj nom = *Sp == N(c_d_bind) ? Sp[1] : nil;
  with(nom, with(x, k = ccc(v, e, m+2)));
  with(k,
    x = homp(x = ltu(v, e, nom, x)) ? x :
    (j = toplp(e) || !twop(loc(*e)) ? encln : encll,
     c_la_clo(v, e, X(x), Y(x))));
  return em2(j, x, k); }

c2(c_imm) {
  return Push(N(immv), x), insx(v, e, m); }

static obj c_la_clo(vm v, mem e, obj arg, obj seq) {
  num i = llen(arg);
  mm(&arg), mm(&seq);
  for (Push(N(insx), N(take), N(i), N(c_ini));
       twop(arg);
       Push(N(c_ev), X(arg), N(inst), N(push)), arg = Y(arg));
  return arg = ccc(v, e, 0), um, um, pair(v, seq, arg); }

c1(c_d_bind) {
  obj y = *Sp++;
  return toplp(e) ? imx(v, e, m, tbind, y) :
                    imx(v, e, m, setl, N(idx(loc(*e), y))); }

c1(c_ev_d) {
  obj w = *Sp++, y;
  mm(&w);
  if (toplp(e) || -1 < idx(loc(*e), X(w))) Push(N(c_d_bind), X(w));
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
         em2(rec, a, k); }

static obj topl_lookup(vm v, obj y) {
  obj q = tbl_get(v, Dict, y);
  return q ? q : tbl_get(v, v->cdict, y); }


#define L(n,x) pair(v, N(n), x)
static obj look(vm v, obj e, obj y) {
  obj q;
  if (nilp(e)) return (q = topl_lookup(v, y)) ?
    L(Here, q) : L(Wait, Dict);
  if ((q = idx(loc(e), y)) != -1) return L(Loc, e);
  if ((q = idx(arg(e), y)) != -1) return L(Arg, e);
  if ((q = idx(clo(e), y)) != -1) return L(Clo, e);
  return look(v, par(e), y); }
#undef L

static obj imx(vm v, mem e, num m, terp *i, obj x) {
  return Push(N(i), x), insx(v, e, m); }

c2(late, obj d) {
  obj k;
  x = pair(v, d, x);
  with(x, k = ccc(v, e, m+2));
  with(k, x = pair(v, N(8), x));
  return em2(lbind, x, k); }

c2(c_sy) {
  obj y, q;
  with(x, y = X(q = look(v, *e, x)));
  switch (Gn(y)) {
    case Here: return c_imm(v, e, m, Y(q));
    case Wait: return late(v, e, m, x, Y(q));
    default:
      if (Y(q) == *e) switch (Gn(y)) {
        case Loc : return imx(v, e, m, locn, N(idx(loc(*e), x)));
        case Arg : return imx(v, e, m, argn, N(idx(arg(*e), x)));
        case Clo : return imx(v, e, m, clon, N(idx(clo(*e), x))); }
      y = llen(clo(*e));
      with(x, q = snoc(v, clo(*e), x)), clo(*e) = q;
      return imx(v, e, m, clon, N(y)); } }

c2(c_qt) { return c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }

c2(c_2) {
  obj z = X(x);
  return (z == Qt ? c_qt :
          z == If ? c_co :
          z == De ? c_de :
          z == La ? c_la :
          z == Se ? c_se :
                    c_ap)(v, e, m, x); }

c2(c_ap) {
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
static num idx(obj l, obj x) {
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
  return va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs), puttup(t); }

static void pushss(vm v, num i, va_list xs) {
  obj x = va_arg(xs, obj);
  if (x) with(x, pushss(v, i+1, xs)), *--Sp = x;
  else if (Avail < i) reqsp(v, i); }

static void pushs(vm v, ...) {
  va_list xs; va_start(xs, v), pushss(v, 0, xs), va_end(xs); }

obj hom_ini(vm v, num n) {
  hom a = cells(v, n + 2);
  return G(a+n) = NULL,
         GF(a+n) = (terp*) a,
         memset(a, -1, w2b(n)),
         puthom(a+n); }

obj hom_fin(vm v, obj a) {
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
         twop(x) ? x : nil; }

static void rpr(vm v, mem d, const char *n, terp *u) {
  obj x, y = pair(v, interns(v, n), nil);
  with(y, x = hom_ini(v, 2));
  x = em2(u, y, x);
  tbl_set(v, *d, X(y), x); }
static void rin(vm v, mem d, const char *n, terp *u) {
  obj y = interns(v, n);
  tbl_set(v, *d, y, putnum(u)); }

#define prims(_)\
  _("read", rd_u),\
  _(".", em_u),        _("ns", globs),\
  _("cns", cglobs),\
  _("*:", car_u),     _(":*", cdr_u),\
  _("*!", setcar_u), _("!*", setcdr_u),\
  _("::", cons_u),   _("=", eq_u),\
  _("<", lt_u),        _("<=", lteq_u),\
  _(">", gt_u),        _(">=", gteq_u),\
  _("+", add_u),      _("-", sub_u),\
  _("*", mul_u),      _("/", div_u),\
  _("%", mod_u),      _("ap", ap_u),\
  _("ccc", ccc_u),     _("ev", ev_u),\
  _("||", or_u),       _("&&", and_u),\
  _("fail", fail_u), _("tbl", tblmk),\
  _("tbl-get", tblg),  _("tbl-set", tbls),\
  _("tbl-has", tblc),  _("tbl-del", tbld),\
  _("tbl-keys", tblks),_("tbl-len", tbll),\
  _("str-len", strl),  _("str-get", strg),\
  _("str", strmk),     _(".c", pc_u),\
  _("hom", hom_u),     _("hom-seek", hom_seek_u),\
  _("hom-fin", hom_fin_u),\
  _("hom-set-x", hom_setx_u), _("hom-get-x", hom_getx_u),\
  _("hom-set-i", hom_seti_u), _("hom-get-i", hom_geti_u),\
  _("zzz", zzz),       _("nump", nump_u),\
  _("symp", symp_u), _("twop", twop_u),\
  _("tblp", tblp_u), _("strp", strp_u),\
  _("nilp", nilp_u), _("homp", homp_u)

#define RPR(a,b) rpr(v,&d,a,b)
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
  if (!v) errp(v, "init", 0, "oom");
  else if (setjmp(v->restart))
    finalize(v), v = NULL;
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
