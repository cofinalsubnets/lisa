#include "lips.h"
#include "terp.h"
#include "hom.h"
#include "tbl.h"
#include "str.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "io.h"

////
/// bootstrap thread compiler
//
// functions construct their continuations by pushing function
// pointers onto the main stack with Push
#define PushCc(...) pushs(v,__VA_ARGS__,(obj)0)
// and then calling them with Ccc
#define Ccc ((c1*)N(*Sp++))
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
static obj hfin(lips, obj), hini(lips, u64), compile_lambda_clo(lips, mem, obj, obj), ltu(lips, mem, obj, obj);
typedef obj c1(lips, mem, u64), c2(lips, mem, u64, obj);
static c1 compile_expression_, pre_bind, emit_instruction, emit_instruction_with_data, compile_begin;
static c2 compile_expression, compile_variable, compile_combination, compile_immediate;

enum { Here, Loc, Arg, Clo, Wait };
#define CO(nom,...) static obj nom(lips v, mem e, u64 m, ##__VA_ARGS__)
#define c1(nom,...) static obj nom(lips v, mem e, u64 m, ##__VA_ARGS__)
#define c2(nom,...) static obj nom(lips v, mem e, u64 m, obj x, ##__VA_ARGS__)

// helper functions for lists
static i64 lidx(obj l, obj x) {
 for (i64 i = 0; twop(l); l = Y(l), i++)
  if (x == X(l)) return i;
 return -1; }

static obj linitp(lips v, obj x, mem d) {
 obj y; return !twop(Y(x)) ? (*d = x, nil) :
  (with(x, y = linitp(v, Y(x), d)), pair(v, X(x), y)); }

static obj snoc(lips v, obj l, obj x) {
 return !twop(l) ? pair(v, x, l) :
  (with(l, x = snoc(v, Y(l), x)), pair(v, X(l), x)); }

static u0 pushss(lips v, i64 i, va_list xs) {
 obj x = va_arg(xs, obj);
 x ? (with(x, pushss(v, i, xs)), *--Sp = x) : reqsp(v, i); }

static u0 pushs(lips v, ...) {
 i64 i = 0;
 va_list xs; va_start(xs, v);
 while (va_arg(xs, obj)) i++;
 va_end(xs), va_start(xs, v);
 if (Avail < i) pushss(v, i, xs);
 else for (mem sp = Sp -= i; i--; *sp++ = va_arg(xs, obj));
 va_end(xs); }

// emit code backwards like cons
static obj em1(terp *i, obj k) {
 return k -= W, G(k) = i, k; }

static obj em2(terp *i, obj j, obj k) {
 return em1(i, em1((terp*)j, k)); }

static obj imx(lips v, mem e, i64 m, terp *i, obj x) {
 PushCc(_N(i), x);
 return emit_instruction_with_data(v, e, m); }

static NoInline obj apply(lips v, obj f, obj x) {
 PushCc(f, x);
 hom h = cells(v, 5);
 h[0] = call;
 h[1] = (terp*) Pn(2);
 h[2] = yield;
 h[3] = NULL;
 h[4] = (terp*) h;
 return call(v, Ph(h), Fp, Sp, Hp, tbl_get(v, Top, App)); }

static NoInline obj rwlade(lips v, obj x) {
 mm(&x);
 for (obj y; twop(X(x));
  y = snoc(v, Y(X(x)), X(Y(x))),
  y = pair(v, v->glob[Lamb], y),
  y = pair(v, y, Y(Y(x))),
  x = pair(v, X(X(x)), y));
 return um, x; }

static int scan_def(lips v, mem e, obj x) {
 if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
 if (!twop(Y(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
 mm(&x);
 int r = scan_def(v, e, Y(Y(x)));
 if (r) {
  x = rwlade(v, x);
  obj y = pair(v, X(x), loc(*e));
  loc(*e) = y;
  scan(v, e, X(Y(x))); }
 return um, r; }

static u0 scan(lips v, mem e, obj x) {
 if (!twop(x) || X(x) == La || X(x) == Qt) return;
 if (X(x) == De) return (u0) scan_def(v, e, Y(x));
 for (mm(&x); twop(x); x = Y(x)) scan(v, e, X(x));
 um; }

static obj asign(lips v, obj a, i64 i, mem m) {
 obj x;
 if (!twop(a)) return *m = i, a;
 if (twop(Y(a)) && X(Y(a)) == Va)
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
 va_start(xs, v);
 t = tuplr(v, 0, xs);
 va_end(xs);
 return putvec(t); }

static Inline obj scope(lips v, mem e, obj a, obj n) {
 i64 s = 0;
 return with(n, a = asign(v, a, 0, &s)),
        tupl(v, a, nil, nil, e ? *e : nil, n, Pn(s), (obj)0); }

static Inline obj compose(lips v, mem e, obj x) {
 PushCc(_N(compile_expression_), x,
      _N(emit_instruction), _N(ret),
      _N(compile_begin));
 scan(v, e, Sp[1]);
 x = Ccc(v, e, 4); // 4 = 2 + 2
 i64 i = llen(loc(*e));
 if (i) x = em2(locals, Pn(i), x);
 i = N(asig(*e));
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
          with(l, n = pair(v, n, e ? name(*e) : nil)),
          n = scope(v, e, l, n)),
  l = compose(v, &n, X(y)));
 return l; }

CO(compile_lambda, obj x) {
 terp* j = imm;
 obj k, nom = *Sp == Pn(pre_bind) ? Sp[1] : nil;
 with(nom, with(x, k = Ccc(v, e, m+2)));
 mm(&k);
 if (twop(x = ltu(v, e, nom, x))) {
   j = e && twop(loc(*e)) ? encll : encln;
   x = compile_lambda_clo(v, e, X(x), Y(x)); }
 um;
 return em2(j, x, k); }

CO(compile_immediate, obj x) { return PushCc(Pn(imm), x), emit_instruction_with_data(v, e, m); }

static obj compile_lambda_clo(lips v, mem e, obj arg, obj seq) {
 i64 i = llen(arg);
 mm(&arg), mm(&seq);
 for (PushCc(Pn(emit_instruction_with_data), Pn(take), Pn(i), Pn(compile_begin));
      twop(arg);
      PushCc(Pn(compile_expression_), X(arg), Pn(emit_instruction), Pn(push)), arg = Y(arg));
 return arg = Ccc(v, e, 0), um, um, pair(v, seq, arg); }

CO(pre_bind) { obj y = *Sp++; return
 e ? imx(v, e, m, loc_, Pn(lidx(loc(*e), y))) :
     imx(v, e, m, tbind, y); }

static u0 compile_let_r(lips v, mem e, obj x) {
 if (twop(x))
  x = rwlade(v, x),
  with(x, compile_let_r(v, e, Y(Y(x)))),
  PushCc(Pn(compile_expression_), X(Y(x)), Pn(pre_bind), X(x)); }

// syntactic sugar for define
static obj def_sug(lips v, obj x) {
 obj y = nil; return
  with(y, x = linitp(v, x, &y)),
  x = pair(v, x, y),   x = pair(v, Se, x),
  x = pair(v, x, nil), x = pair(v, La, x),
  pair(v, x, nil); }

CO(compile_let, obj x) { return
 !twop(Y(x))    ? compile_immediate(v, e, m, nil) :
 llen(Y(x)) % 2 ? compile_expression(v, e, m, def_sug(v, x)) :
                  (compile_let_r(v, e, Y(x)), Ccc(v, e, m)); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 v->xp
#define S2 v->ip

// before generating anything, store the
// exit address in stack 2
CO(compile_conditional_pre) {
 obj x = Ccc(v, e, m);
 return X(S2 = pair(v, x, S2)); }

// before generating a branch emit a jump to
// the top of stack 2
CO(compile_conditional_pre_con) {
 obj x = Ccc(v, e, m + 2), k = X(S2);
 return G(k) == ret ? em1(ret, x) : em2(jump, k, x); }

// after generating a branch store its address
// in stack 1
CO(compile_conditional_post_con) {
 obj x = Ccc(v, e, m);
 return X(S1 = pair(v, x, S1)); }

// before generating an antecedent emit a branch to
// the top of stack 1
CO(compile_conditional_pre_ant) {
 obj x = Ccc(v, e, m+2);
 return x = em2(branch, X(S1), x), S1 = Y(S1), x; }

static u0 compile_conditional_loop(lips v, mem e, obj x) {
 if (!twop(x)) x = pair(v, nil, nil);

 if (!twop(Y(x)))
   PushCc(
     _N(compile_expression_), X(x),
     _N(compile_conditional_pre_con));
 else {
   with(x,
     PushCc(
       _N(compile_conditional_post_con),
       _N(compile_expression_), X(Y(x)),
       _N(compile_conditional_pre_con)),
     compile_conditional_loop(v, e, Y(Y(x))));
   PushCc(
     Pn(compile_expression_), X(x),
     _N(compile_conditional_pre_ant)); } }

CO(compile_conditional, obj x) {
  with(x, PushCc(_N(compile_conditional_pre)));
  compile_conditional_loop(v, e, Y(x));
  x = Ccc(v, e, m);
  S2 = Y(S2);
  return x; }

CO(emit_call) {
 obj a = *Sp++, k = Ccc(v, e, m + 2);
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

CO(compile_variable, obj x) {
  obj y, q;
  with(x, y = X(q = lookup_variable(v, e ? *e:nil, x)));
  switch (N(y)) {
    case Here: return compile_immediate(v, e, m, Y(q));
    case Wait:
      x = pair(v, Y(q), x);
      with(x, y = Ccc(v, e, m+2));
      with(y, x = pair(v, _N(8), x));
      return em2(lbind, x, y);
    default:
      if (Y(q) == *e) switch (N(y)) {
        case Loc: return imx(v, e, m, loc, _N(lidx(loc(*e), x)));
        case Arg: return imx(v, e, m, arg, _N(lidx(arg(*e), x)));
        default:  return imx(v, e, m, clo, _N(lidx(clo(*e), x))); }
      y = llen(clo(*e));
      with(x, q = snoc(v, clo(*e), x));
      clo(*e) = q;
      return imx(v, e, m, clo, _N(y)); } }

c1(compile_expression_) { return compile_expression(v, e, m, *Sp++); }
c2(compile_expression) { return (symp(x) ? compile_variable : twop(x) ? compile_combination : compile_immediate)(v, e, m, x); }


CO(compile_macro, obj macro, obj args) {
  obj s1 = S1, s2 = S2;
  with(s1, with(s2, macro = apply(v, macro, args)));
  S1 = s1, S2 = s2;
  return compile_expression(v, e, m, macro); }

CO(compile_apply, obj fun, obj args) {
  // function call
  mm(&args);
  PushCc(
    _N(compile_expression_), fun,
    _N(emit_instruction), _N(idH),
    _N(emit_call), _N(llen(args)));
  while (twop(args))
    PushCc(
      N_(compile_expression_), X(args),
      N_(emit_instruction), N_(push)),
    args = Y(args);
  um;

  return Ccc(v, e, m); }

static u0 compile_sequence_loop(lips v, mem e, obj x) {
  if (twop(x))
    with(x, compile_sequence_loop(v, e, Y(x))),
    PushCc(_N(compile_expression_), X(x)); }

c2(compile_combination) {
  obj z = X(x);
  if (z == If) return compile_conditional(v, e, m, x);
  if (z == De) return compile_let(v, e, m, x);
  if (z == La) return compile_lambda(v, e, m, x);
  if (z == Se) {
    if (!twop(x = Y(x))) x = pair(v, x, nil);
    compile_sequence_loop(v, e, x);
    return Ccc(v, e, m); }
  if (z == Qt) {
    x = twop(x = Y(x)) ? X(x) : x;
    return compile_immediate(v, e, m, x); }
  if ((z = tbl_get(v, Mac, z)))
    return compile_macro(v, e, m, z, Y(x));
  return compile_apply(v, e, m, X(x), Y(x)); }

c1(emit_instruction) {
 terp* i = (terp*) N(*Sp++);
 return em1(i, Ccc(v, e, m+1)); }

c1(emit_instruction_with_data) {
 terp* i = (terp*) N(*Sp++);
 obj x = *Sp++, k;
 return with(x, k = Ccc(v, e, m+2)), em2(i, x, k); }

c1(compile_begin) {
 obj k = hini(v, m+1);
 return em1((terp*)(e ? name(*e) : Eva), k); }

static NoInline obj hini(lips v, u64 n) {
 hom a = cells(v, n + 2);
 a[n] = NULL;
 a[n+1] = (terp*) a;
 set64((mem) a, nil, n);
 return _H(a+n); }

static obj hfin(lips v, obj a) {
 return (obj) (GF(button(Gh(a))) = (terp*) a); }

obj eval(lips v, obj x) {
  obj args = pair(v, x, nil),
      ev = tbl_get(v, Top, Eva);
  return apply(v, ev, args); }

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
 GO(ret, Ph(h+len-2)); }

VM(hfin_u) {
 ARY(1);
 obj a = *ARGV;
 TC(a, Hom);
 GF(button(Gh(a))) = (terp*) a;
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

VM(hgeti_u) { ARY(1); TC(ARGV[0], Hom); GO(ret,   N_(G(ARGV[0]))); }
VM(hgetx_u) { ARY(1); TC(ARGV[0], Hom); GO(ret, (obj)G(ARGV[0])); }

VM(hseek_u) {
 ARY(2); TC(ARGV[0], Hom); TC(ARGV[1], Num);
 GO(ret, H_(H(ARGV[0])+N(ARGV[1]))); }

VM(ev_u) {
  ARY(1);
  RETC(
    PushCc(_N(emit_instruction), _N(yield),
           _N(compile_begin)),
    xp = compile_expression(v, NULL, 0, *ARGV),
    v->xp = G(xp)(v, xp, v->fp, v->sp, v->hp, nil)); }

GC(cphom) {
 hom src = gethom(x);
 if (fresh(G(src))) return (obj) G(src);
 hom end = button(src), start = (hom) G(end+1),
     dst = bump(v, end - start + 2), j = dst;
 for (hom k = start; k < end;
  G(j) = G(k),
  G(k++) = (terp*) puthom(j++));
 G(j) = NULL;
 G(j+1) = (terp*) dst;
 for (obj u; j-- > dst;
   u = (obj) G(j),
   G(j) = (terp*) (!stale(u) ? u : cp(v, u, len0, base0)));
 return puthom(dst += src - start); }

// finalize function instance closure
static VM(pc1) { G(ip) = clos; GF(ip) = (terp*) xp; NEXT(0); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static VM(pc0) {
 obj ec  = (obj) GF(ip),
     arg = V(ec)->xs[0],
     loc = V(ec)->xs[1];
 u64 adic = nilp(arg) ? 0 : V(arg)->len;
 Have(Size(frame) + adic + 1);
 i64 off = (mem) fp - sp;
 G(ip) = pc1;
 sp -= adic;
 cpy64(sp, V(arg)->xs, adic);
 ec = (obj) GF(ip);
 fp = sp -= Size(frame);
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
 t->xs[3] = Y(x);
 t->xs[4] = Ph(at);

 at[0] = pc0;
 at[1] = (terp*) putvec(t);
 at[2] = (terp*) X(x);
 at[3] = 0;
 at[4] = (terp*) at;

 AP(ip+W2, Ph(at)); }

VM(encll) { GO(encl, LOCS); }
VM(encln) { GO(encl, nil); }

NoInline obj homnom(lips v, obj x) {
  terp *k = G(x);
  if (k == clos || k == pc0 || k == pc1)
    return homnom(v, (obj) G(FF(x)));
  mem h = (mem) Gh(x);
  while (*h) h++;
  x = h[-1];
  int inb = (mem) x >= v->pool && (mem) x < v->pool+v->len;
  return inb ? x : x == (obj) yield ? Eva : nil; }
