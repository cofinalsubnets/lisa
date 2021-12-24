#include "lips.h"
#include "hom.h"
#include "tbl.h"
#include "str.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "write.h"
#include "vec.h"
#include "terp.h"

////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Push
#define PushCc(...) pushs(v,__VA_ARGS__,NULL)
// and then calling them with Ccc
#define Ccc(m) ((c1*)N(*v->sp++))(v,e,m)
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
#define arg(x)  V(x)->xs[0] // argument variables : a list
#define loc(x)  V(x)->xs[1] // local variables : a list
#define clo(x)  V(x)->xs[2] // closure variables : a list
#define par(x)  V(x)->xs[3] // surrounding scope : tuple or nil
#define name(x) V(x)->xs[4] // function name : a symbol or nil
#define asig(x) V(x)->xs[5] // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.

static u0 scan(lips, mem, obj);
static obj comp_lambda_clo(lips, mem, obj, obj), ltu(lips, mem, obj, obj);
typedef obj c1(lips, mem, u64), c2(lips, mem, u64, obj);
static c1 comp_expr_, comp_let_bind, emit_i, emit_i_d, comp_alloc_thread;
static c2 comp_expr, comp_sym, comp_list, comp_imm;

enum { Here, Loc, Arg, Clo, Wait };
#define CO(nom,...) static obj nom(lips v, mem e, u64 m, ##__VA_ARGS__)

// helper functions for lists
static i64 lidx(obj l, obj x) {
 for (i64 i = 0; twop(l); l = B(l), i++)
  if (x == A(l)) return i;
 return -1; }

static obj linitp(lips v, obj x, mem d) {
 obj y; return !twop(B(x)) ? (*d = x, nil) :
  (with(x, y = linitp(v, B(x), d)), pair(v, A(x), y)); }

static obj snoc(lips v, obj l, obj x) {
  return !twop(l) ? pair(v, x, l) :
    (with(l, x = snoc(v, B(l), x)), pair(v, A(l), x)); }

static u0 pushss(lips v, i64 i, va_list xs) {
  obj x = va_arg(xs, obj);
  if (x)
    with(x,  pushss(v, i+1, xs)),
    *--v->sp = x;
  else if (Avail < i && !cycle(v, i))
    errp(v, "oom"),
    restart(v); }

static u0 pushs(lips v, ...) {
  va_list xs;
  va_start(xs, v), pushss(v, 0, xs), va_end(xs); }

static vec tuplr(lips v, i64 i, va_list xs) {
 vec t;
 obj x = va_arg(xs, obj);
 if (x)
   with(x, t = tuplr(v, i+1, xs)),
   t->xs[i] = x;
 else
  t = cells(v, Width(vec) + i),
  t->len = i;
 return t; }

static obj tupl(lips v, ...) {
 vec t;
 va_list xs;
 va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs);
 return _V(t); }

// emit code backwards like cons
static obj em1(terp *i, obj k) {
 return k -= W, G(k) = i, k; }

static obj em2(terp *i, obj j, obj k) {
 return em1(i, em1((terp*)j, k)); }

static obj imx(lips v, mem e, i64 m, terp *i, obj x) {
 PushCc(_N(i), x);
 return emit_i_d(v, e, m); }

static NoInline obj rw_let_fn(lips v, obj x) {
 mm(&x);
 for (obj y; twop(A(x));)
  y = snoc(v, BA(x), AB(x)),
  y = pair(v, La, y),
  y = pair(v, y, BB(x)),
  x = pair(v, AA(x), y);
 return um, x; }

static bool scan_def(lips v, mem e, obj x) {
 if (!twop(x)) return true; // this is an even case so export all the definitions to the local scope
 if (!twop(B(x))) return false; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 bool r = scan_def(v, e, BB(x));
 if (r) {
  x = rw_let_fn(v, x);
  obj y = pair(v, A(x), loc(*e));
  loc(*e) = y;
  scan(v, e, AB(x)); }
 return um, r; }

static u0 scan(lips v, mem e, obj x) {
 if (!twop(x) || A(x) == La || A(x) == Qt) return;
 if (A(x) == De) return (u0) scan_def(v, e, B(x));
 mm(&x);
 while (twop(x)) scan(v, e, A(x)), x = B(x);
 um; }

static obj asign(lips v, obj a, i64 i, mem m) {
 obj x;
 if (!twop(a)) return *m = i, a;
 if (twop(B(a)) && AB(a) == Va) return
   *m = -(i+1),
   pair(v, A(a), nil);
 return
   with(a, x = asign(v, B(a), i+1, m)),
   pair(v, A(a), x); }

static Inline obj scope(lips v, mem e, obj a, obj n) {
 i64 s = 0;
 return with(n, a = asign(v, a, 0, &s)),
        tupl(v, a, nil, nil, e ? *e : nil, n, _N(s), (obj)0); }

static Inline obj compose(lips v, mem e, obj x) {
 PushCc(
   _N(comp_expr_), x,
   _N(emit_i), _N(ret),
   _N(comp_alloc_thread));
 scan(v, e, v->sp[1]);
 x = Ccc(4); // 4 = 2 + 2
 i64 i = llen(loc(*e));
 if (i) x = em2(locals, _N(i), x);
 i = N(asig(*e));
 if (i > 0) x = em2(arity, _N(i), x);
 else if (i < 0) x = em2(vararg, _N(-i-1), x);
 GF(button(H(x))) = (terp*) x;
 return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static obj ltu(lips v, mem e, obj n, obj l) {
 obj y;
 l = B(l);
 with(n,
  l = twop(l) ? l : pair(v, l, nil),
  with(y, l = linitp(v, l, &y),
          with(l, n = pair(v, n, e ? name(*e) : nil)),
          n = scope(v, e, l, n)),
  l = compose(v, &n, A(y)));
 return l; }

CO(comp_lambda, obj x) {
 terp* j = imm;
 obj k, nom = *v->sp == _N(comp_let_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = Ccc(m+2)));
 mm(&k);
 if (twop(x = ltu(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = comp_lambda_clo(v, e, A(x), B(x));
 um;
 return em2(j, x, k); }

CO(comp_imm, obj x) {
  PushCc(_N(imm), x);
  return emit_i_d(v, e, m); }

static obj comp_lambda_clo(lips v, mem e, obj arg, obj seq) {
  i64 i = llen(arg);
  mm(&arg), mm(&seq);

  for (PushCc(
         _N(emit_i_d), _N(take), _N(i),
         _N(comp_alloc_thread));
       twop(arg);
       arg = B(arg))
    PushCc(
      _N(comp_expr_), A(arg),
      _N(emit_i), _N(push));

  arg = Ccc(0);
  um, um;
  return pair(v, seq, arg); }

CO(comp_let_bind) {
  obj y = *v->sp++;
  return e ? imx(v, e, m, loc_, _N(lidx(loc(*e), y))) :
             imx(v, e, m, tbind, y); }

static u0 comp_let_r(lips v, mem e, obj x) {
 if (twop(x))
  x = rw_let_fn(v, x),
  with(x, comp_let_r(v, e, BB(x))),
  PushCc(_N(comp_expr_), AB(x), _N(comp_let_bind), A(x)); }

// syntactic sugar for define
static obj def_sug(lips v, obj x) {
  obj y = nil;
  return with(y, x = linitp(v, x, &y)),
         x = pair(v, x, y),   x = pair(v, Se, x),
         x = pair(v, x, nil), x = pair(v, La, x),
         pair(v, x, nil); }

CO(comp_let, obj x) { return
 !twop(B(x))    ? comp_imm(v, e, m, nil) :
 llen(B(x)) % 2 ? comp_expr(v, e, m, def_sug(v, x)) :
                  (comp_let_r(v, e, B(x)), Ccc(m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 v->xp
#define S2 v->ip

// before generating anything, store the
// exit address in stack 2
CO(comp_if_pre) {
 obj x = Ccc(m);
 return A(S2 = pair(v, x, S2)); }

// before generating a branch emit a jump to
// the top of stack 2
CO(comp_if_pre_con) {
 obj x = Ccc(m + 2), k = A(S2);
 return G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
CO(comp_if_post_con) {
 obj x = Ccc(m);
 return A(S1 = pair(v, x, S1)); }

// before generating an antecedent emit a branch to
// the top of stack 1
CO(comp_if_pre_ant) {
 obj x = Ccc(m+2);
 return x = em2(branch, A(S1), x), S1 = B(S1), x; }

static u0 comp_if_loop(lips v, mem e, obj x) {
 if (!twop(x)) x = pair(v, nil, nil);

 if (!twop(B(x)))
   PushCc(
     _N(comp_expr_), A(x),
     _N(comp_if_pre_con));

 else {
   with(x,
     PushCc(
       _N(comp_if_post_con),
       _N(comp_expr_), AB(x),
       _N(comp_if_pre_con)),
     comp_if_loop(v, e, BB(x)));
   PushCc(
     _N(comp_expr_), A(x),
     _N(comp_if_pre_ant)); } }

CO(comp_if, obj x) {
  with(x, PushCc(_N(comp_if_pre)));
  comp_if_loop(v, e, B(x));
  x = Ccc(m);
  S2 = B(S2);
  return x; }

CO(emit_call) {
  obj a = *v->sp++, k = Ccc(m + 2);
  return em2(G(k) == ret ? rec : call, a, k); }

#define L(n,x) pair(v, _N(n), x)
static obj lookup_variable(lips v, obj e, obj y) {
  if (nilp(e)) {
    obj q = tbl_get(v, Top, y);
    return q ? L(Here, q) : L(Wait, Top); }
  return
    lidx(loc(e), y) > -1 ? L(Loc, e) :
    lidx(arg(e), y) > -1 ? L(Arg, e) :
    lidx(clo(e), y) > -1 ? L(Clo, e) :
    lookup_variable(v, par(e), y); }
#undef L

CO(comp_sym, obj x) {
  obj y, q;
  with(x, y = A(q = lookup_variable(v, e ? *e:nil, x)));
  switch (N(y)) {
    case Here: return comp_imm(v, e, m, B(q));
    case Wait:
      x = pair(v, B(q), x);
      with(x, y = Ccc(m+2));
      with(y, x = pair(v, _N(8), x));
      return em2(lbind, x, y);
    default:
      if (B(q) == *e) switch (N(y)) {
        case Loc:
          return imx(v, e, m, loc, _N(lidx(loc(*e), x)));
        case Arg:
          return imx(v, e, m, arg, _N(lidx(arg(*e), x)));
        default:
          return imx(v, e, m, clo, _N(lidx(clo(*e), x))); }
      y = llen(clo(*e));
      with(x, q = snoc(v, clo(*e), x));
      clo(*e) = q;
      return imx(v, e, m, clo, _N(y)); } }

CO(comp_expr_) {
  return comp_expr(v, e, m, *v->sp++); }

CO(comp_expr, obj x) {
  return (symp(x) ? comp_sym :
          twop(x) ? comp_list :
                    comp_imm)(v, e, m, x); }

static obj apply(lips, obj, obj) NoInline;

CO(comp_macro, obj macro, obj args) {
  obj s1 = S1, s2 = S2;
  with(s1, with(s2, macro = apply(v, macro, args)));
  S1 = s1, S2 = s2;
  return comp_expr(v, e, m, macro); }

CO(comp_call, obj fun, obj args) {
  // function call
  mm(&args);
  PushCc(
    _N(comp_expr_), fun,
    _N(emit_i), _N(idH),
    _N(emit_call), _N(llen(args)));
  while (twop(args))
    PushCc(
      _N(comp_expr_), A(args),
      _N(emit_i), _N(push)),
    args = B(args);
  um;

  return Ccc(m); }

static u0 comp_sequence_loop(lips v, mem e, obj x) {
  if (twop(x))
    with(x, comp_sequence_loop(v, e, B(x))),
    PushCc(_N(comp_expr_), A(x)); }

CO(comp_list, obj x) {
  obj z = A(x);
  if (z == If) return comp_if(v, e, m, x);
  if (z == De) return comp_let(v, e, m, x);
  if (z == La) return comp_lambda(v, e, m, x);
  if (z == Se) {
    if (!twop(x = B(x))) x = pair(v, x, nil);
    comp_sequence_loop(v, e, x);
    return Ccc(m); }
  if (z == Qt) {
    x = twop(x = B(x)) ? A(x) : x;
    return comp_imm(v, e, m, x); }
  if ((z = tbl_get(v, Mac, z)))
    return comp_macro(v, e, m, z, B(x));
  return comp_call(v, e, m, A(x), B(x)); }

CO(emit_i) {
 terp* i = (terp*) N(*v->sp++);
 return em1(i, Ccc(m+1)); }

CO(emit_i_d) {
 terp* i = (terp*) N(*v->sp++);
 obj x = *v->sp++, k;
 return with(x, k = Ccc(m+2)), em2(i, x, k); }

static obj hini(lips v, u64 n) {
 hom a = cells(v, n + 2);
 a[n] = NULL;
 a[n+1] = (terp*) a;
 set64((mem) a, nil, n);
 return _H(a+n); }

CO(comp_alloc_thread) {
 obj k = hini(v, m+1);
 return em1((terp*)(e ? name(*e) : Eva), k); }

obj eval(lips v, obj x) {
  obj args = pair(v, x, nil),
      ev = tbl_get(v, Top, Eva);
  return apply(v, ev, args); }

static NoInline obj apply(lips v, obj f, obj x) {
 PushCc(f, x);
 hom h = cells(v, 5);
 h[0] = call;
 h[1] = (terp*)_N(2);
 h[2] = yield;
 h[3] = NULL;
 h[4] = (terp*) h;
 f = _H(h), x = tbl_get(v, Top, App);
 return call(v, f, v->fp, v->sp, v->hp, x); }

GC(cphom) {
 hom src = H(x);
 if (fresh(G(src))) return (obj) G(src);
 hom end = button(src), start = (hom) G(end+1),
     dst = bump(v, end - start + 2), j = dst;
 for (hom k = start; k < end;)
  G(j) = G(k),
  G(k++) = (terp*) _H(j++);
 G(j) = NULL;
 G(j+1) = (terp*) dst;
 for (obj u; j-- > dst;
   u = (obj) G(j),
   G(j) = (terp*) (!stale(u) ? u : cp(v, u, len0, base0)));
 return _H(dst += src - start); }

// VM instructions
// instructions used by the compiler
VM(hom_u) {
 obj x;
 ARY(1);
 TC(x = *ARGV, Num);
 i64 len = N(x) + 2;
 Have(len);
 hom h = (hom) hp;
 hp += len;
 set64((mem) h, nil, len);
 h[len-1] = (terp*) h;
 h[len-2] = NULL;
 GO(ret, _H(h+len-2)); }

VM(hfin_u) {
 ARY(1);
 obj a = *ARGV;
 TC(a, Hom);
 GF(button(H(a))) = (terp*) a;
 GO(ret, a); }

VM(emx) { obj h = *sp++ - W; G(h) = (terp*) xp;    AP(ip+W, h); }
VM(emi) { obj h = *sp++ - W; G(h) = (terp*) N(xp); AP(ip+W, h); }

VM(emx_u) {
 ARY(2);
 obj h = ARGV[1];
 TC(h, Hom);
 h -= W;
 G(h) = (terp*) ARGV[0];
 GO(ret, h); }

VM(emi_u) {
 ARY(2);
 TC(ARGV[0], Num);
 obj h = ARGV[1];
 TC(h, Hom);
 h -= W;
 G(h) = (terp*) N(ARGV[0]);
 GO(ret, h); }

VM(hgeti_u) { ARY(1); TC(ARGV[0], Hom); GO(ret,   _N(G(ARGV[0]))); }
VM(hgetx_u) { ARY(1); TC(ARGV[0], Hom); GO(ret, (obj)G(ARGV[0])); }

VM(hseek_u) {
 ARY(2); TC(ARGV[0], Hom); TC(ARGV[1], Num);
 GO(ret, _H(H(ARGV[0])+N(ARGV[1]))); }

VM(ev_u) {
  ARY(1);
  RETC(
    PushCc(_N(emit_i), _N(yield),
           _N(comp_alloc_thread)),
    xp = comp_expr(v, NULL, 0, *ARGV),
    v->xp = G(xp)(v, xp, v->fp, v->sp, v->hp, nil)); }

static VM(clos) { CLOS = (obj) GF(ip); AP((obj) G(FF(ip)), xp); }
// finalize function instance closure
static VM(clos1) { G(ip) = clos; GF(ip) = (terp*) xp; NEXT(0); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static VM(clos0) {
 obj ec  = (obj) GF(ip),
     arg = V(ec)->xs[0],
     loc = V(ec)->xs[1];
 u64 adic = nilp(arg) ? 0 : V(arg)->len;
 Have(Width(frame) + adic + 1);
 i64 off = (mem) fp - sp;
 G(ip) = clos1;
 sp -= adic;
 cpy64(sp, V(arg)->xs, adic);
 ec = (obj) GF(ip);
 fp = sp -= Width(frame);
 RETP = ip;
 SUBR = _N(off);
 ARGC = _N(adic);
 CLOS = V(ec)->xs[2];
 if (!nilp(loc)) *--sp = loc;
 ip = V(ec)->xs[3];
 NEXT(0); }

// the next few functions create and store
// lexical environments.
static VM(encl) {
 i64 n = N(ARGC);
 n += n ? 12 : 11;
 Have(n);
 obj x = (obj) GF(ip), arg = nil;
 mem block = hp;
 hp += n;
 if (n > 11) {
  n -= 12;
  vec t = (vec) block;
  block += 1 + n;
  t->len = n;
  while (n--) t->xs[n] = ARGV[n];
  arg = putvec(t); }

 vec t = (vec) block; // compiler thread closure array (1 length 5 elements)
 hom at = (hom) (block+6); // compiler thread (1 instruction 2 data 2 tag)

 t->len = 5; // initialize alpha closure
 t->xs[0] = arg;
 t->xs[1] = xp; // LOCS or nil
 t->xs[2] = CLOS;
 t->xs[3] = B(x);
 t->xs[4] = _H(at);

 at[0] = clos0;
 at[1] = (terp*) putvec(t);
 at[2] = (terp*) A(x);
 at[3] = 0;
 at[4] = (terp*) at;

 AP(ip+W2, _H(at)); }

VM(encll) { GO(encl, LOCS); }
VM(encln) { GO(encl, nil); }

NoInline obj homnom(lips v, obj x) {
  terp *k = G(x);
  if (k == clos || k == clos0 || k == clos1)
    return homnom(v, (obj) G(FF(x)));
  mem h = (mem) H(x);
  while (*h) h++;
  x = h[-1];
  int inb = (mem) x >= v->pool && (mem) x < v->pool+v->len;
  return inb ? x : x == (obj) yield ? Eva : nil; }

VM(hnom_u) {
  ARY(1);
  TC(*ARGV, Hom);
  xp = homnom(v, *ARGV);
  Jump(ret); }
