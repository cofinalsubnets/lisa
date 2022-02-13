#include "lips.h"
#include "hom.h"
#include "tbl.h"
#include "str.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "vec.h"
#include "terp.h"
#include "write.h"


////
/// bootstrap thread compiler
//
// passes continuations by pushing function pointers
// onto the main stack with Push and calling them with Ccc
#define Push(...) pushs(v,__VA_ARGS__,NULL)
#define Ccc(m) ((c1*)N(*v->sp++))(v,e,m)
// also uses Xp and Ip as stacks for storing code entry points
// when generating conditionals.
//
// this compiler emits runtime type checks for safety but does
// (almost) no optimizations or static typing since all it has
// to do is bootstrap the main compiler.


// " compilation environments "
typedef struct { obj arg, loc, clo, par, nom, sig; } *cenv;
#define Ce(x) ((cenv)(V(x)->xs))
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define arg(x)  Ce(x)->arg // argument variables : a list
#define loc(x)  Ce(x)->loc // local variables : a list
#define clo(x)  Ce(x)->clo // closure variables : a list
#define par(x)  Ce(x)->par // surrounding scope : tuple or nil
#define name(x) Ce(x)->nom // function name : a symbol or nil
#define asig(x) Ce(x)->sig // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.

static u0 scan(lips, mem, obj);
static obj comp_lambda_clo(lips, mem, obj, obj), ltu(lips, mem, obj, obj);
typedef hom c1(lips, mem, u64), c2(lips, mem, u64, obj);
static c1 comp_expr_, comp_let_bind, emit_i, emit_i_d, comp_alloc;
static c2 comp_expr, comp_sym, comp_list, comp_imm;

enum { Here, Loc, Arg, Clo, Wait };
#define CO(nom,...) static hom nom(lips v, mem e, u64 m, ##__VA_ARGS__)
#define inptr(x) _N((i64)(x))

// helper functions for lists
static i64 lidx(obj l, obj x) {
  for (i64 i = 0; twop(l); l = B(l), i++) if (x == A(l)) return i;
  return -1; }

static obj linitp(lips v, obj x, mem d) {
  obj y;
  if (!twop(B(x))) return *d = x, nil;
  with(x, y = linitp(v, B(x), d));
  bind(y, y);
  return pair(v, A(x), y); }

static obj snoc(lips v, obj l, obj x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = snoc(v, B(l), x));
  bind(x, x);
  return pair(v, A(l), x); }

static u1 pushss(lips v, i64 i, va_list xs) {
  u1 _;
  obj x = va_arg(xs, obj);
  if (!x) return Avail >= i || please(v, i); 
  with(x, _ = pushss(v, i+1, xs));
  bind(_, _);
  *--v->sp = x;
  return _; }

static u1 pushs(lips v, ...) {
  va_list xs;
  va_start(xs, v);
  u1 _ = pushss(v, 0, xs);
  va_end(xs);
  return _; }

static vec tuplr(lips v, i64 i, va_list xs) {
 vec t;
 obj x = va_arg(xs, obj);
 if (!x) {
   bind(t, cells(v, Width(vec) + i));
   t->len = i; }
 else {
   with(x, t = tuplr(v, i+1, xs));
   bind(t, t);
   t->xs[i] = x; }
 return t; }

static obj tupl(lips v, ...) {
 vec t;
 va_list xs;
 va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs);
 bind(t, t);
 return _V(t); }

// emit code backwards like cons
static Inline obj em1(terp *i, obj k) {
 return k -= W, *H(k) = i, k; }

static Inline obj em2(terp *i, obj j, obj k) {
 return em1(i, em1((terp*)j, k)); }

static Inline hom ee1(terp *i, hom k) {
 return *--k = i, k; }

static Inline hom ee2(terp *i, obj x, hom k) {
 return ee1(i, ee1((terp*) x, k)); }

static Inline hom imx(lips v, mem e, i64 m, terp *i, obj x) {
  bind(x, Push(inptr(i), x));
  return emit_i_d(v, e, m); }

#define Bind(v, x) if(!((v)=(x)))goto fail
static NoInline obj rw_let_fn(lips v, obj x) {
  obj y;
  for (mm(&x); twop(A(x));) {
    Bind(y, snoc(v, BA(x), AB(x)));
    Bind(y, pair(v, La, y));
    Bind(y, pair(v, y, BB(x)));
    Bind(x, pair(v, AA(x), y)); }
  return um, x; fail:
  return um, 0; }

static u1 scan_def(lips v, mem e, obj x) {
  if (!twop(x)) return true; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return false; // this is an odd case so ignore these, they'll be imported after the rewrite
  mm(&x);
  u1 r = scan_def(v, e, BB(x));
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
 for (; twop(x); x = B(x)) scan(v, e, A(x));
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

static Inline obj new_scope(lips v, mem e, obj a, obj n) {
 i64 s = 0;
 return with(n, a = asign(v, a, 0, &s)),
        tupl(v, a, nil, nil, e ? *e : nil, n, _N(s), (obj)0); }

static Inline obj comp_body(lips v, mem e, obj x) {
 bind(x, Push(
   inptr(comp_expr_), x,
   inptr(emit_i), inptr(ret),
   inptr(comp_alloc)));
 scan(v, e, v->sp[1]);
 bind(x, _H(Ccc(4))); // 4 = 2 + 2
 i64 i = llen(loc(*e));
 if (i) x = em2(locals, _N(i), x);
 i = N(asig(*e));
 if (i > 0) x = em2(arity, _N(i), x);
 else if (i < 0) x = em2(vararg, _N(-i-1), x);
 button(H(x))[1] = (terp*) x;
 return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
//
// FIXME fallible
static Inline obj ltu(lips v, mem e, obj n, obj l) {
 obj y;
 l = B(l);
 with(n,
  l = twop(l) ? l : pair(v, l, nil),
  with(y, l = linitp(v, l, &y),
          with(l, n = pair(v, n, e ? name(*e) : nil)),
          n = new_scope(v, e, l, n)),
  l = comp_body(v, &n, A(y)));
 return l; }

// FIXME fallible
static Inline obj comp_lambda_clo(lips v, mem e, obj arg, obj seq) {
  i64 i = llen(arg);
  mm(&arg), mm(&seq);

  for (Push(
         inptr(emit_i_d), inptr(take), _N(i),
         inptr(comp_alloc));
       twop(arg);
       arg = B(arg))
    Push(
      inptr(comp_expr_), A(arg),
      inptr(emit_i), inptr(push));

  arg = _H(Ccc(0));
  um, um;
  bind(arg, arg);
  return pair(v, seq, arg); }

CO(comp_lambda, obj x) {
 terp* j = imm;
 obj k, nom = *v->sp == inptr(comp_let_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = _H(Ccc(m+2))));
 bind(k, k);
 mm(&k);
 if (twop(x = ltu(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = comp_lambda_clo(v, e, A(x), B(x));
 um;
 return ee2(j, x, H(k)); }

CO(comp_imm, obj x) {
  bind(x, Push(inptr(imm), x));
  return emit_i_d(v, e, m); }

CO(comp_let_bind) {
  obj y = *v->sp++;
  return e ? imx(v, e, m, loc_, _N(lidx(loc(*e), y))) :
             imx(v, e, m, tbind, y); }

static u1 comp_let_r(lips v, mem e, obj x) {
  u1 _ = true;
  if (twop(x)) {
    bind(x, rw_let_fn(v, x));
    with(x, _ = comp_let_r(v, e, BB(x)));
    bind(_, _);
    bind(_, Push(inptr(comp_expr_), AB(x), inptr(comp_let_bind), A(x))); }
  return _; }

// syntactic sugar for define
static Inline obj def_sug(lips v, obj x) {
  obj y = nil;
  with(y, x = linitp(v, x, &y));
  bind(x, x);
  bind(x, pair(v, x, y));
  bind(x, pair(v, Se, x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, La, x));
  return pair(v, x, nil); }

CO(comp_let, obj x) {
  if (!twop(B(x))) return comp_imm(v, e, m, nil);
  if (llen(B(x)) % 2) {
    bind(x, def_sug(v, x));
    return comp_expr(v, e, m, x); }
  bind(x, comp_let_r(v, e, B(x)));
  return Ccc(m); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 v->xp
#define S2 v->ip

// before generating anything, store the
// exit address in stack 2
CO(comp_if_pre) {
 obj x;
 bind(x, _H(Ccc(m)));
 bind(x, pair(v, x, S2));
 return H(A(S2 = x)); }

// before generating a branch emit a jump to
// the top of stack 2
CO(comp_if_pre_con) {
  hom x, k;
  bind(x, Ccc(m + 2));
  k = H(A(S2));
  return *k == ret ? ee1(ret, x) : ee2(jump, _H(k), x); }

// after generating a branch store its address
// in stack 1
CO(comp_if_post_con) {
 obj x;
 bind(x, _H(Ccc(m)));
 bind(x, pair(v, x, S1));
 return H(A(S1 = x)); }

// before generating an antecedent emit a branch to
// the top of stack 1
CO(comp_if_pre_ant) {
 hom x;
 bind(x, Ccc(m+2));
 x = ee2(branch, A(S1), x);
 S1 = B(S1);
 return x; }

static u1 comp_if_loop(lips v, mem e, obj x) {
  if (!twop(x)) bind(x, pair(v, nil, nil));

  if (!twop(B(x))) return
    Push(
      inptr(comp_expr_), A(x),
      inptr(comp_if_pre_con));

  u1 _;
  with(x,
    _ = Push(
      inptr(comp_if_post_con),
      inptr(comp_expr_), AB(x),
      inptr(comp_if_pre_con)));
  bind(_, _);
  with(x, comp_if_loop(v, e, BB(x)));
  bind(_, _);
  return Push(
    inptr(comp_expr_), A(x),
    inptr(comp_if_pre_ant)); }

CO(comp_if, obj x) {
  u1 _;
  with(x, _ = Push(inptr(comp_if_pre)));
  bind(_, _);
  bind(_, comp_if_loop(v, e, B(x)));
  hom k;
  bind(k, Ccc(m));
  S2 = B(S2);
  return k; }

CO(emit_call) {
  obj a = *v->sp++;
  hom k;
  bind(k, Ccc(m + 2));
  return ee2(*k == ret ? rec : call, a, k); }

obj lookup_mod(lips v, obj x) {
  return tbl_get(v, Top, x); }

static obj lookup_lex(lips v, obj e, obj y) {
  if (nilp(e)) {
    obj q = lookup_mod(v, y);
    return q ? pair(v, _N(Here), q) : pair(v, _N(Wait), Top); }
  return
    lidx(loc(e), y) > -1 ? pair(v, _N(Loc), e) :
    lidx(arg(e), y) > -1 ? pair(v, _N(Arg), e) :
    lidx(clo(e), y) > -1 ? pair(v, _N(Clo), e) :
    lookup_lex(v, par(e), y); }

CO(comp_sym, obj x) {
  obj y, q;
  with(x, q = lookup_lex(v, e ? *e:nil, x));
  bind(q, q);
  y = A(q);
  switch (N(y)) {
    case Here: return comp_imm(v, e, m, B(q));
    case Wait:
      bind(x, pair(v, B(q), x));
      with(x, y = _H(Ccc(m+2)));
      bind(y, y);
      with(y, x = pair(v, _N(8), x));
      bind(x, x);
      return ee2(lbind, x, H(y));
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
      bind(q, q);
      clo(*e) = q;
      return imx(v, e, m, clo, _N(y)); } }

CO(comp_expr_) { return comp_expr(v, e, m, *v->sp++); }
CO(comp_expr, obj x) { return (symp(x) ? comp_sym :
                               twop(x) ? comp_list :
                                         comp_imm)(v, e, m, x); }

static obj apply(lips, obj, obj) NoInline;

// FIXME no macros in stage 1
CO(comp_macro, obj macro, obj args) {
  obj s1 = S1, s2 = S2;
  with(s1, with(s2, macro = apply(v, macro, args)));
  bind(macro, macro);
  S1 = s1, S2 = s2;
  return comp_expr(v, e, m, macro); }

CO(comp_call, obj fun, obj args) {
  mm(&args);
  Bind(fun, Push(
    inptr(comp_expr_), fun,
    inptr(emit_i), inptr(idH),
    inptr(emit_call), _N(llen(args))));
  while (twop(args)) {
    Bind(fun, Push(
      inptr(comp_expr_), A(args),
      inptr(emit_i), inptr(push)));
    args = B(args); }

  return um, Ccc(m); fail:
  return um, NULL; }

static u1 comp_seq_loop(lips v, mem e, obj x) {
  u1 _ = true;
  if (twop(x)) {
    with(x, _ = comp_seq_loop(v, e, B(x)));
    bind(_, _);
    bind(_, Push(inptr(comp_expr_), A(x))); }
  return _; }

CO(comp_list, obj x) {
  obj z = A(x);
  if (z == If) return comp_if(v, e, m, x);
  if (z == De) return comp_let(v, e, m, x);
  if (z == La) return comp_lambda(v, e, m, x);
  if (z == Se) {
    if (!twop(x = B(x))) bind(x, pair(v, x, nil));
    bind(x, comp_seq_loop(v, e, x));
    return Ccc(m); }
  if (z == Qt) {
    x = twop(x = B(x)) ? A(x) : x;
    return comp_imm(v, e, m, x); }
  // FIXME bootstrap compiler should not use macros
  if ((z = tbl_get(v, Mac, z)))
    return comp_macro(v, e, m, z, B(x));
  return comp_call(v, e, m, A(x), B(x)); }

CO(emit_i) {
 terp* i = (terp*) N(*v->sp++);
 hom k;
 bind(k, Ccc(m+1));
 return ee1(i, k); }

CO(emit_i_d) {
 terp* i = (terp*) N(*v->sp++);
 obj x = *v->sp++;
 hom k;
 with(x, k = Ccc(m+2));
 bind(k, k);
 return ee2(i, x, k); }

static hom hini(lips v, u64 n) {
 hom a;
 bind(a, cells(v, n + 2));
 a[n] = NULL;
 a[n+1] = (terp*) a;
 set64((mem) a, nil, n);
 return a + n; }

CO(comp_alloc) {
 hom k;
 bind(k, hini(v, m+1));
 return ee1((terp*)(e ? name(*e) : nil), k); }

obj eval(lips v, obj x) {
  obj args;
  bind(args, pair(v, x, nil));
  return apply(v, homp(Eva) ? Eva : tbl_get(v, Top, Eva), args); }

static NoInline obj apply(lips v, obj f, obj x) {
 Push(f, x);
 hom h;
 bind(h, cells(v, 5));
 h[0] = call;
 h[1] = (terp*)_N(2);
 h[2] = yield;
 h[3] = NULL;
 h[4] = (terp*) h;
 f = _H(h), x = tbl_get(v, Top, App);
 return call(v, f, v->fp, v->sp, v->hp, x); }

// instructions used by the compiler
Vm(hom_u) {
 obj x;
 Ary(1);
 Tc(x = *Argv, Num);
 i64 len = N(x) + 2;
 Have(len);
 hom h = (hom) hp;
 hp += len;
 set64((mem) h, nil, len);
 h[len-1] = (terp*) h;
 h[len-2] = NULL;
 Go(ret, _H(h+len-2)); }

Vm(hfin_u) {
 Ary(1);
 obj a = *Argv;
 Tc(a, Hom);
 button(H(a))[1] = (terp*) a;
 Go(ret, a); }

Vm(emx) { hom h = H(*sp++ - W); *h = (terp*) xp;    Ap(ip+W, _H(h)); }
Vm(emi) { hom h = H(*sp++ - W); *h = (terp*) N(xp); Ap(ip+W, _H(h)); }

Vm(emx_u) {
 Ary(2);
 obj h = Argv[1];
 Tc(h, Hom);
 h -= W;
 *H(h) = (terp*) Argv[0];
 Go(ret, h); }

Vm(emi_u) {
 Ary(2);
 Tc(Argv[0], Num);
 obj h = Argv[1];
 Tc(h, Hom);
 h -= W;
 *H(h) = (terp*) N(Argv[0]);
 Go(ret, h); }

Vm(hgeti_u) { Ary(1); Tc(Argv[0], Hom); Go(ret, inptr(*H(Argv[0]))); }
Vm(hgetx_u) { Ary(1); Tc(Argv[0], Hom); Go(ret, (obj) *H(Argv[0])); }

Vm(hseek_u) {
  Ary(2);
  Tc(Argv[0], Hom);
  Tc(Argv[1], Num);
  Go(ret, _H(H(Argv[0]) + N(Argv[1]))); }

obj analyze(lips v, obj x) {
  with(x, Push(inptr(emit_i), inptr(ret), inptr(comp_alloc)));
  return _H(comp_expr(v, NULL, 0, x)); }

Vm(ev_u) {
  Ary(1);
  if (homp(Eva)) ip = Eva;
  else { Pack();
         bind(v->ip, analyze(v, *Argv));
         Unpack(); }
  Next(0); }

Vm(bootstrap) {
  Ary(1);
  xp = *Argv;
  Tc(xp, Hom);
  // neither intern nor tbl_set will allocate if ev is already interned / defined
  tbl_set(v, Top, interns(v, "ev"), Eva = xp);
  Jump(ret); }

static Vm(clos) { Clos = (obj) H(ip)[1]; Ap((obj) H(ip)[2], xp); }
// finalize function instance closure
static Vm(clos1) { *H(ip) = clos; H(ip)[1] = (terp*) xp; Next(0); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static Vm(clos0) {
 obj ec  = (obj) H(ip)[1],
     arg = V(ec)->xs[0],
     loc = V(ec)->xs[1];
 u64 adic = nilp(arg) ? 0 : V(arg)->len;
 Have(Width(frame) + adic + 1);
 i64 off = (mem) fp - sp;
 *H(ip) = clos1;
 sp -= adic;
 cpy64(sp, V(arg)->xs, adic);
 ec = (obj) H(ip)[1];
 fp = sp -= Width(frame);
 Retp = ip;
 Subr = _N(off);
 Argc = _N(adic);
 Clos = V(ec)->xs[2];
 if (!nilp(loc)) *--sp = loc;
 ip = V(ec)->xs[3];
 Next(0); }

// the next few functions create and store
// lexical environments.
static Vm(encl) {
 i64 n = N(Argc);
 n += n ? 12 : 11;
 Have(n);
 obj x = (obj) H(ip)[1], arg = nil;
 mem block = hp;
 hp += n;
 if (n > 11) {
  n -= 12;
  vec t = (vec) block;
  block += 1 + n;
  t->len = n;
  while (n--) t->xs[n] = Argv[n];
  arg = putvec(t); }

 vec t = (vec) block; // compiler thread closure array (1 length 5 elements)
 hom at = (hom) (block+6); // compiler thread (1 instruction 2 data 2 tag)

 t->len = 5; // initialize alpha closure
 t->xs[0] = arg;
 t->xs[1] = xp; // Locs or nil
 t->xs[2] = Clos;
 t->xs[3] = B(x);
 t->xs[4] = _H(at);

 at[0] = clos0;
 at[1] = (terp*) putvec(t);
 at[2] = (terp*) A(x);
 at[3] = 0;
 at[4] = (terp*) at;

 Ap(ip+W2, _H(at)); }

Vm(encll) { Go(encl, Locs); }
Vm(encln) { Go(encl, nil); }

NoInline obj homnom(lips v, obj x) {
  terp *k = *H(x);
  if (k == clos || k == clos0 || k == clos1)
    return homnom(v, (obj) H(x)[2]);
  mem h = (mem) H(x);
  while (*h) h++;
  x = h[-1];
  int inb = (mem) x >= v->pool && (mem) x < v->pool+v->len;
  return inb ? x : nil; }

Vm(hnom_u) {
  Ary(1);
  Tc(*Argv, Hom);
  xp = homnom(v, *Argv);
  Jump(ret); }

obj sequence(lips v, obj a, obj b) {
  hom h;
  with(a, with(b, h = cells(v, 8)));
  bind(h, h);
  h[0] = imm;
  h[1] = (terp*) a;
  h[2] = call;
  h[3] = (terp*) _N(0);
  h[4] = jump;
  h[5] = (terp*) b;
  h[6] = NULL;
  h[7] = (terp*) h;
  return _H(h); }
