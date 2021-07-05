#include "lips.h"
#include "terp.h"
////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Push
#define Push(...) pushs(v,__VA_ARGS__,non)
// and then calling them with Ccc
#define Ccc ((c1*)Gn(*Sp++))
// there's a natural correspondence between the push/Ccc pattern
// in this file and normal continuation passing style in lisp
// (cf. the stage 2 compiler).

// in addition to the main stack, the compiler uses Xp and Ip
// as stacks for storing code entry points when generating
// conditionals, which is admittedly kind of sus.
//
// this compiler emits runtime type checks for safety but does
// (almost) no optimizations or static typing since all it has
// to do is bootstrap the main compiler.

// " compilation environments "
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define toplp(x) !e
#define arg(x)  AR(x)[0] // argument variables : a list
#define loc(x)  AR(x)[1] // local variables : a list
#define clo(x)  AR(x)[2] // closure variables : a list
#define par(x)  AR(x)[3] // surrounding scope : tuple or nil
#define name(x) AR(x)[4] // function name : a symbol or nil
#define asig(x) AR(x)[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.
//

typedef obj
 c1(lips, mem, u64),
 c2(lips, mem, u64, obj),
 c3(lips, mem, u64, obj, obj);
static u0
 c_de_r(lips, mem, obj),
 scan(lips, mem, obj),
 pushs(lips, ...);
static obj
 linitp(lips, obj, mem),
 snoc(lips, obj, obj),
 look(lips, obj, obj),
 hfin(lips, obj),
 hini(lips, u64);
static c1 c_ev, c_d_bind, inst, insx, c_ini;
static c2 c_eval, c_sy, c_2, c_imm, c_ap;
static obj c_la_clo(lips, mem, obj, obj), ltu(lips, mem, obj, obj);
static c3 late;
static i64 lidx(obj, obj);

enum { Here, Loc, Arg, Clo, Wait };
#define c1(nom,...) static obj nom(lips v, mem e, u64 m, ##__VA_ARGS__)
#define c2(nom,...) static obj nom(lips v, mem e, u64 m, obj x, ##__VA_ARGS__)

// emit code backwards like cons
static obj em1(terp *i, obj k) {
 return k -= W, G(k) = i, k; }
static obj em2(terp *i, obj j, obj k) {
 return em1(i, em1((terp*)j, k)); }

static obj imx(lips v, mem e, i64 m, terp *i, obj x) {
 return Push(Pn(i), x), insx(v, e, m); }

static NoInline obj apply(lips v, obj f, obj x) {
 Push(f, x);
 hom h = cells(v, 5);
 h[0] = call;
 h[1] = (terp*) Pn(2);
 h[2] = yield;
 h[3] = NULL;
 h[4] = (terp*) h;
 return call(v, Ph(h), Fp, Sp, Hp, tblget(v, Dict, App)); }

static NoInline obj rwlade(lips v, obj x) {
 obj y;
 mm(&x);
 while (twop(X(x)))
  y = snoc(v, YX(x), XY(x)),
  y = pair(v, La, y),
  y = pair(v, y, YY(x)),
  x = pair(v, XX(x), y) ;
 return um, x; }

static int scan_def(lips v, mem e, obj x) {
 if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
 if (!twop(Y(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 int r = scan_def(v, e, YY(x));
 if (r) {
  x = rwlade(v, x);
  obj y = pair(v, X(x), loc(*e));
  loc(*e) = y;
  scan(v, e, XY(x)); }
 return um, r; }

static u0 scan(lips v, mem e, obj x) {
 if (!twop(x) || X(x) == La || X(x) == Qt) return;
 if (X(x) == De) return (u0) scan_def(v, e, Y(x));
 for (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
 um; }

static obj asign(lips v, obj a, i64 i, mem m) {
 obj x;
 if (!twop(a)) return *m = i, a;
 if (twop(Y(a)) && XY(a) == Va)
  return *m = -(i+1), pair(v, X(a), nil);
 with(a, x = asign(v, Y(a), i+1, m));
 return pair(v, X(a), x); }

static vec tuplr(lips v, i64 i, va_list xs) {
 vec t; obj x;
 return (x = va_arg(xs, obj)) ?
  (with(x, t = tuplr(v, i+1, xs)), t->xs[i] = x, t) :
  ((t = cells(v, Size(tup) + i))->len = i, t); }

static obj tupl(lips v, ...) {
 vec t; va_list xs;
 return
  va_start(xs, v),
  t = tuplr(v, 0, xs),
  va_end(xs),
  puttup(t); }

static Inline obj scope(lips v, mem e, obj a, obj n) {
 i64 s = 0;
 return with(n, a = asign(v, a, 0, &s)),
        tupl(v, a, nil, nil, e ? *e : nil, n, Pn(s), non); }

static Inline obj compose(lips v, mem e, obj x) {
 Push(Pn(c_ev), x, Pn(inst), Pn(ret), Pn(c_ini));
 scan(v, e, Sp[1]);
 x = Ccc(v, e, 4); // 4 = 2 + 2
 i64 i = llen(loc(*e));
 if (i) x = em2(locals,  Pn(i), x);
 i = Gn(asig(*e));
 if (i > 0) x = em2(arity, Pn(i), x);
 else if (i < 0) x = em2(vararg, Pn(-i-1), x);
 x = hfin(v, x);
 return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expression, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static obj ltu(lips v, mem e, obj n, obj l) {
 obj y;
 l = Y(l);
 with(n,
  l = twop(l) ? l : pair(v, l, nil),
  with(y, l = linitp(v, l, &y),
          with(l, n = pair(v, n, toplp(e) ? nil : e ? name(*e):nil)),
          n = scope(v, e, l, n)),
  l = compose(v, &n, X(y)));
 return l; }

c2(c_la) {
 terp* j = imm;
 obj k, nom = *Sp == Pn(c_d_bind) ? Sp[1] : nil;
 with(nom, with(x, k = Ccc(v, e, m+2)));
 with(k,
  x = homp(x = ltu(v, e, nom, x)) ? x :
  (j = toplp(e) || !twop(loc(*e)) ? encln : encll,
   c_la_clo(v, e, X(x), Y(x))));
 return em2(j, x, k); }

c2(c_imm) { return Push(Pn(imm), x), insx(v, e, m); }

static obj c_la_clo(lips v, mem e, obj arg, obj seq) {
 i64 i = llen(arg);
 mm(&arg), mm(&seq);
 for (Push(Pn(insx), Pn(take), Pn(i), Pn(c_ini));
      twop(arg);
      Push(Pn(c_ev), X(arg), Pn(inst), Pn(push)), arg = Y(arg));
 return arg = Ccc(v, e, 0), um, um,
        pair(v, seq, arg); }

c1(c_d_bind) {
 obj y = *Sp++;
 return toplp(e) ? imx(v, e, m, tbind, y) :
                   imx(v, e, m, loc_, Pn(lidx(loc(*e), y))); }

static u0 c_de_r(lips v, mem e, obj x) {
 if (!twop(x)) return;
 x = rwlade(v, x);
 with(x, c_de_r(v, e, YY(x))),
 Push(Pn(c_ev), XY(x), Pn(c_d_bind), X(x)); }

// syntactic sugar for define
static obj def_sug(lips v, obj x) {
 obj y = nil;
 with(y, x = linitp(v, x, &y));
 x = pair(v, x, y),   x = pair(v, Se, x);
 x = pair(v, x, nil), x = pair(v, La, x);
 return pair(v, x, nil); }

c2(c_de) { return
 !twop(Y(x))    ? c_imm(v, e, m, nil) :
 llen(Y(x)) % 2 ? c_eval(v, e, m, def_sug(v, x)) :
                  (c_de_r(v, e, Y(x)), Ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 Xp
#define S2 Ip

// before generating anything, store the
// exit address in stack 2
c1(c_co_pre) {
 obj x = Ccc(v, e, m);
 return X(S2 = pair(v, x, S2)); }

// before generating a branch emit a jump to
// the top of stack 2
c1(c_co_pre_con) {
 obj x = Ccc(v, e, m+2), k = X(S2);
 return G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
c1(c_co_post_con) {
 obj x = Ccc(v, e, m);
 return X(S1 = pair(v, x, S1)); }

// before generating an antecedent emit a branch to
// the top of stack 1
c1(c_co_pre_ant) {
 obj x = Ccc(v, e, m+2);
 return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static u0 c_co_r(lips v, mem e, obj x) {
 if (!twop(x)) x = pair(v, nil, nil);
 if (!twop(Y(x))) Push(Pn(c_ev), X(x), Pn(c_co_pre_con));
 else with(x, Push(Pn(c_co_post_con), Pn(c_ev), XY(x),
                   Pn(c_co_pre_con)),
              c_co_r(v, e, YY(x))),
      Push(Pn(c_ev), X(x), Pn(c_co_pre_ant)); }

c2(c_co) { return
 with(x, Push(Pn(c_co_pre))),
 c_co_r(v, e, Y(x)),
 x = Ccc(v, e, m),
 S2 = Y(S2),
 x; }

static u0 c_se_r(lips v, mem e, obj x) {
 if (twop(x)) with(x, c_se_r(v, e, Y(x))),
              Push(Pn(c_ev), X(x)); }
c2(c_se) {
 if (!twop(x = Y(x))) x = pair(v, nil, nil);
 return c_se_r(v, e, x), Ccc(v, e, m); }

c1(c_call) {
 obj a = *Sp++, k = Ccc(v, e, m + 2);
 return em2(G(k) == ret ? rec : call, a, k); }

#define L(n,x) pair(v, Pn(n), x)
static obj look(lips v, obj e, obj y) {
 obj q;
 return
  nilp(e) ?
   ((q = tblget(v, Dict, y)) ?  L(Here, q) : L(Wait, Dict)) :
  ((q = lidx(loc(e), y)) != -1) ?
   L(Loc, e) :
  ((q = lidx(arg(e), y)) != -1) ?
   L(Arg, e) :
  ((q = lidx(clo(e), y)) != -1) ?
   L(Clo, e) :
  look(v, par(e), y); }
#undef L

c2(late, obj d) {
 obj k;
 return
  x = pair(v, d, x),
  with(x, k = Ccc(v, e, m+2)),
  with(k, x = pair(v, Pn(8), x)),
  em2(lbind, x, k); }

c2(c_sy) {
 obj y, q;
 with(x, y = X(q = look(v, e ? *e:nil, x)));
 switch (Gn(y)) {
  case Here: return c_imm(v, e, m, Y(q));
  case Wait: return late(v, e, m, x, Y(q));
  default:
   if (Y(q) == *e) switch (Gn(y)) {
    case Loc: return imx(v, e, m, loc, Pn(lidx(loc(*e), x)));
    case Arg: return imx(v, e, m, arg, Pn(lidx(arg(*e), x)));
    case Clo: return imx(v, e, m, clo, Pn(lidx(clo(*e), x))); }
   y = llen(clo(*e));
   with(x, q = snoc(v, clo(*e), x)), clo(*e) = q;
   return imx(v, e, m, clo, Pn(y)); } }

c1(c_ev) { return c_eval(v, e, m, *Sp++); }
c2(c_eval) {
 return (symp(x)?c_sy:twop(x)?c_2:c_imm)(v,e,m,x); }

c2(c_qt) { return c_imm(v, e, m, twop(x = Y(x)) ? X(x) : x); }
c2(c_2) {
 obj z = X(x);
 return
  (z==Qt?c_qt:
   z==If?c_co:
   z==De?c_de:
   z==La?c_la:
   z==Se?c_se:c_ap)(v,e,m,x); }

c2(c_ap) {
 obj mac = tblget(v, Mac, X(x));
 if (mac) {
  obj s1 = S1, s2 = S2;
  with(s1, with(s2, x = apply(v, mac, Y(x))));
  S1 = s1, S2 = s2;
  return c_eval(v, e, m, x); }
 for (mm(&x),
      Push(Pn(c_ev), X(x), Pn(inst), Pn(idH),
           Pn(c_call), Pn(llen(Y(x))));
      twop(x = Y(x));
      Push(Pn(c_ev), X(x), Pn(inst), Pn(push)));
 return um, Ccc(v, e, m); }

c1(inst) {
 terp* i = (terp*) Gn(*Sp++);
 return em1(i, Ccc(v, e, m+1)); }

c1(insx) {
 terp* i = (terp*) Gn(*Sp++);
 obj x = *Sp++, k;
 with(x, k = Ccc(v, e, m+2));
 return em2(i, x, k); }

c1(c_ini) {
 obj k = hini(v, m+1);
 return em1((terp*)(e ? name(*e):Eva), k); }

static u0 pushss(lips v, i64 i, va_list xs) {
 obj x = va_arg(xs, obj);
 x ? (with(x, pushss(v, i, xs)), *--Sp = x) :
     reqsp(v, i); }

static u0 pushs(lips v, ...) {
 i64 i = 0;
 va_list xs; va_start(xs, v);
 while (va_arg(xs, obj)) i++;
 va_end(xs), va_start(xs, v);
 if (Avail < i) pushss(v, i, xs);
 else for (mem sp = Sp -= i; i--; *sp++ = va_arg(xs, obj));
 va_end(xs); }

static NoInline obj hini(lips v, u64 n) {
 hom a = cells(v, n + 2);
 return
  G(a+n) = NULL,
  GF(a+n) = (terp*) a,
  set64((mem) a, nil, n),
  Ph(a+n); }

static obj hfin(lips v, obj a) {
 return (obj) (GF(button(Gh(a))) = (terp*) a); }

NoInline obj homnom(lips v, obj x) {
 terp *k = G(x);
 if (k == clos || k == pc0 || k == pc1)
  return homnom(v, (obj) G(FF(x)));
 mem h = (mem) Gh(x);
 while (*h) h++;
 x = h[-1];
 return (mem) x >= Pool && (mem) x < Pool+Len ? x :
        x == (obj) yield                      ? Eva :
                                                nil; }

NoInline u0 defprim(lips v, const char *a, terp *b) {
 obj z = spush(v, pair(v, interns(v, a), nil)), x = hini(v, 2);
 x = em2(b, z = spop(v), x);
 tblset(v, *Sp, X(z), x); }

NoInline obj spush(lips v, obj x) {
 if (!Avail) with(x, reqsp(v, 1));
 return *--Sp = x; }

obj compile(lips v, obj x) {
 Push(Pn(c_ev), x, Pn(inst), Pn(yield), Pn(c_ini));
 return Ccc(v, NULL, 0); }

obj eval(lips v, obj x) {
 x = pair(v, x, nil);
 return apply(v, tblget(v, Dict, Eva), x); }

static i64 lidx(obj l, obj x) {
 for (i64 i = 0; twop(l); l = Y(l), i++)
  if (x == X(l)) return i;
 return -1; }

static obj linitp(lips v, obj x, mem d) {
 if (!twop(Y(x))) return *d = x, nil;
 obj y;
 with(x, y = linitp(v, Y(x), d));
 return pair(v, X(x), y); }

static obj snoc(lips v, obj l, obj x) {
 if (!twop(l)) return pair(v, x, l);
 with(l, x = snoc(v, Y(l), x));
 return pair(v, X(l), x); }
