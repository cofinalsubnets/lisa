#include "lips.h"
#include "terp.h"

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
// minimal optimization and analysis since all it has to do
// is bootstrap the main compiler.


// " compilation environments "
typedef struct { ob arg, loc, clo, par, nom, sig; } *cenv;
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

static u1 scan(run, mem, obj);
static obj yo_yo_clo(run, mem, obj, obj), ltu(run, mem, obj, obj);
typedef yo c1(en, ob*, u64), c2(en, ob*, u64, ob);
static c1 xpn_yo_, let_yo_bind, em_i, em_i_d, mk_yo;
static c2 xpn_yo, var_yo, form_yo, im_yo;

enum { Here, Loc, Arg, Clo, Wait };
#define CO(nom,...) static yo nom(en v, ob* e, u64 m, ##__VA_ARGS__)
#define Put(x) _N((i64)(x))

// helper functions for lists
static i64 lidx(obj l, obj x) {
  for (i64 i = 0; twop(l); l = B(l), i++) if (x == A(l)) return i;
  return -1; }

static obj linitp(run v, obj x, mem d) {
  obj y;
  if (!twop(B(x))) return *d = x, nil;
  with(x, y = linitp(v, B(x), d));
  bind(y, y);
  return pair(v, A(x), y); }

static obj snoc(run v, obj l, obj x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = snoc(v, B(l), x));
  bind(x, x);
  return pair(v, A(l), x); }

static u1 pushss(run v, i64 i, va_list xs) {
  u1 _;
  obj x = va_arg(xs, obj);
  if (!x) return Avail >= i || please(v, i);
  with(x, _ = pushss(v, i+1, xs));
  bind(_, _);
  *--v->sp = x;
  return _; }

static u1 pushs(run v, ...) {
  va_list xs; u1 _;
  va_start(xs, v), _ = pushss(v, 0, xs), va_end(xs);
  return _; }

static vec tuplr(run v, i64 i, va_list xs) {
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

static obj tupl(run v, ...) {
 va_list xs; vec t;
 va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs);
 bind(t, t);
 return _V(t); }

// emit code backwards like cons
SI ob em1(terp *i, ob k) {
  return k -= word, H(k)->ll = (vm*) i, k; }

SI ob em2(terp *i, ob j, ob k) {
  return em1(i, em1((terp*)j, k)); }

SI yo ee1(terp *i, yo k) {
  return (--k)->ll = (vm*) i, k; }

SI yo ee2(terp *i, obj x, yo k) {
  return ee1(i, ee1((terp*) x, k)); }

static yo imx(run v, mem e, i64 m, terp *i, obj x) {
  bind(x, Push(Put(i), x));
  return em_i_d(v, e, m); }

#define Bind(v, x) if(!((v)=(x)))goto fail
static NoInline obj rw_let_fn(run v, obj x) {
  obj y;
  for (mm(&x); twop(A(x));) {
    Bind(y, snoc(v, BA(x), AB(x)));
    Bind(y, pair(v, La, y));
    Bind(y, pair(v, y, BB(x)));
    Bind(x, pair(v, AA(x), y)); }
  return um, x; fail:
  return um, 0; }

static i1 scan_def(run v, mem e, obj x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  mm(&x);
  u1 r = scan_def(v, e, BB(x));
  if (r == 1) {
    Bind(x, rw_let_fn(v, x));
    obj y;
    Bind(y, pair(v, A(x), loc(*e)));
    loc(*e) = y;
    Bind(y, scan(v, e, AB(x))); }
  return um, r; fail:
  return um, -1; }

static u1 scan(run v, mem e, obj x) {
  if (!twop(x) || A(x) == La || A(x) == Qt) return true;
  if (A(x) == De) return scan_def(v, e, B(x)) != -1;
  mm(&x);
  for (; twop(x); x = B(x))
    if (!scan(v, e, A(x))) return um, false;
  return um, true; }

static obj asign(run v, obj a, i64 i, mem m) {
  obj x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == Va) {
    *m = -i-1;
    return pair(v, A(a), nil); }
  with(a, x = asign(v, B(a), i+1, m));
  bind(x, x);
  return pair(v, A(a), x); }

SI obj new_scope(run v, mem e, obj a, obj n) {
  i64 s = 0;
  with(n, a = asign(v, a, 0, &s));
  bind(a, a);
  return tupl(v, a, nil, nil, e ? *e : nil, n, _N(s), (obj)0); }

SI obj comp_body(run v, mem e, obj x) {
  bind(x, Push(Put(xpn_yo_), x,
               Put(em_i), Put(ret),
               Put(mk_yo)));
  scan(v, e, v->sp[1]);
  bind(x, (ob) Ccc(4)); // 4 = 2 + 2
  i64 i = llen(loc(*e));
  if (i) x = em2(locals, _N(i), x);
  i = N(asig(*e));
  if (i > 0) x = em2(arity, _N(i), x);
  else if (i < 0) x = em2(vararg, _N(-i-1), x);
  button(H(x))[1].ll = (vm*) x;
  return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
SI obj ltu(run v, mem e, obj n, obj l) {
  obj y = nil;
  l = B(l);
  mm(&n); mm(&y); mm(&l);
  Bind(l, twop(l) ? l : pair(v, l, nil));
  Bind(l, linitp(v, l, &y));
  Bind(n, pair(v, n, e ? name(*e) : nil));
  Bind(n, new_scope(v, e, l, n));
  Bind(l, comp_body(v, &n, A(y)));
  return um, um, um, l; fail:
  return um, um, um, 0; }

SI obj yo_yo_clo(run v, mem e, obj arg, obj seq) {
  i64 i = llen(arg);
  mm(&arg), mm(&seq);
  u1 _;
  Bind(_, Push(
    Put(em_i_d), Put(take), _N(i),
    Put(mk_yo)));
  while (twop(arg)) {
    Bind(_, Push(
      Put(xpn_yo_), A(arg),
      Put(em_i), Put(push)));
    arg = B(arg); }

  Bind(arg, (ob) Ccc(0));
  return um, um, pair(v, seq, arg); fail:
  return um, um, 0; }

CO(yo_yo, obj x) {
 terp* j = imm;
 obj k, nom = *v->sp == Put(let_yo_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = (ob) Ccc(m+2)));
 bind(k, k);
 mm(&k);
 if (twop(x = ltu(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = yo_yo_clo(v, e, A(x), B(x));
 um;
 bind(x, x);
 return ee2(j, x, (yo) k); }

CO(im_yo, obj x) {
  bind(x, Push(Put(imm), x));
  return em_i_d(v, e, m); }

CO(let_yo_bind) {
  obj y = *v->sp++;
  return e ? imx(v, e, m, loc_, _N(lidx(loc(*e), y))) :
             imx(v, e, m, tbind, y); }

static u1 let_yo_r(run v, mem e, obj x) {
  u1 _ = true;
  if (twop(x)) {
    bind(x, rw_let_fn(v, x));
    with(x, _ = let_yo_r(v, e, BB(x)));
    bind(_, _);
    bind(_, Push(Put(xpn_yo_), AB(x), Put(let_yo_bind), A(x))); }
  return _; }

// syntactic sugar for define
SI obj def_sug(run v, obj x) {
  obj y = nil;
  with(y, x = linitp(v, x, &y));
  bind(x, x);
  bind(x, pair(v, x, y));
  bind(x, pair(v, Se, x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, La, x));
  return pair(v, x, nil); }

CO(let_yo, obj x) {
  if (!twop(B(x))) return im_yo(v, e, m, nil);
  if (llen(B(x)) % 2) {
    bind(x, def_sug(v, x));
    return xpn_yo(v, e, m, x); }
  bind(x, let_yo_r(v, e, B(x)));
  return Ccc(m); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.
#define S1 v->xp
#define S2 v->ip

// before generating anything, store the
// exit address in stack 2
CO(if_yo_pre) {
 obj x;
 bind(x, (ob) Ccc(m));
 bind(x, pair(v, x, S2));
 return (yo) A(S2 = x); }

// before generating a branch emit a jump to
// the top of stack 2
CO(if_yo_pre_con) {
  yo x, k;
  bind(x, Ccc(m + 2));
  k = (yo) A(S2);
  return k->ll == (vm*) ret ? ee1(ret, x) : ee2(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
CO(if_yo_post_con) {
 ob x;
 bind(x, (ob) Ccc(m));
 bind(x, pair(v, x, S1));
 return (yo) A(S1 = x); }

// before generating an antecedent emit a branch to
// the top of stack 1
CO(if_yo_pre_ant) {
 yo x;
 bind(x, Ccc(m+2));
 x = ee2(branch, A(S1), x);
 S1 = B(S1);
 return x; }

static u1 if_yo_loop(run v, mem e, obj x) {
  if (!twop(x)) bind(x, pair(v, nil, nil));

  if (!twop(B(x))) return
    Push(
      Put(xpn_yo_), A(x),
      Put(if_yo_pre_con));

  u1 _;
  with(x,
    _ = Push(
      Put(if_yo_post_con),
      Put(xpn_yo_), AB(x),
      Put(if_yo_pre_con)));
  bind(_, _);
  with(x, _ = if_yo_loop(v, e, BB(x)));
  bind(_, _);
  return Push(
    Put(xpn_yo_), A(x),
    Put(if_yo_pre_ant)); }

CO(if_yo, obj x) {
  u1 _;
  with(x, _ = Push(Put(if_yo_pre)));
  bind(_, _);
  bind(_, if_yo_loop(v, e, B(x)));
  yo k;
  bind(k, Ccc(m));
  S2 = B(S2);
  return k; }

CO(em_call) {
  obj a = *v->sp++;
  yo k;
  bind(k, Ccc(m + 2));
  return ee2(k->ll == (vm*) ret ? rec : call, a, k); }

static obj lookup_mod(run v, obj x) {
  return tbl_get(v, Top, x); }

static obj lookup_lex(run v, obj e, obj y) {
  if (nilp(e)) {
    obj q = lookup_mod(v, y);
    return q ? pair(v, _N(Here), q) : pair(v, _N(Wait), Top); }
  return
    lidx(loc(e), y) > -1 ? pair(v, _N(Loc), e) :
    lidx(arg(e), y) > -1 ? pair(v, _N(Arg), e) :
    lidx(clo(e), y) > -1 ? pair(v, _N(Clo), e) :
    lookup_lex(v, par(e), y); }

CO(var_yo, obj x) {
  obj y, q;
  with(x, q = lookup_lex(v, e ? *e:nil, x));
  bind(q, q);
  y = A(q);
  switch (N(y)) {
    case Here: return im_yo(v, e, m, B(q));
    case Wait:
      bind(x, pair(v, B(q), x));
      with(x, y = (ob) Ccc(m+2));
      bind(y, y);
      with(y, x = pair(v, _N(8), x));
      bind(x, x);
      return ee2(lbind, x, (yo) y);
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

CO(xpn_yo_) { return xpn_yo(v, e, m, *v->sp++); }
CO(xpn_yo, obj x) { return (symp(x) ? var_yo :
                               twop(x) ? form_yo :
                                         im_yo)(v, e, m, x); }

CO(ap_yo, ob fun, ob args) {
  mm(&args);
  Bind(fun, Push(
    Put(xpn_yo_), fun,
    Put(em_i), Put(idH),
    Put(em_call), _N(llen(args))));
  while (twop(args)) {
    Bind(fun, Push(
      Put(xpn_yo_), A(args),
      Put(em_i), Put(push)));
    args = B(args); }

  return um, Ccc(m); fail:
  return um, NULL; }

static u1 seq_yo_loop(run v, mem e, obj x) {
  u1 _ = true;
  if (twop(x)) {
    with(x, _ = seq_yo_loop(v, e, B(x)));
    bind(_, _);
    bind(_, Push(Put(xpn_yo_), A(x))); }
  return _; }

CO(form_yo, obj x) {
  obj z = A(x);
  if (z == If) return if_yo(v, e, m, x);
  if (z == De) return let_yo(v, e, m, x);
  if (z == La) return yo_yo(v, e, m, x);
  if (z == Se) {
    if (!twop(x = B(x))) bind(x, pair(v, x, nil));
    bind(x, seq_yo_loop(v, e, x));
    return Ccc(m); }
  if (z == Qt) {
    x = twop(x = B(x)) ? A(x) : x;
    return im_yo(v, e, m, x); }
  return ap_yo(v, e, m, A(x), B(x)); }

CO(em_i) {
 terp* i = (terp*) N(*v->sp++);
 yo k;
 bind(k, Ccc(m+1));
 return ee1(i, k); }

CO(em_i_d) {
 terp* i = (terp*) N(*v->sp++);
 obj x = *v->sp++;
 yo k;
 with(x, k = Ccc(m+2));
 bind(k, k);
 return ee2(i, x, k); }

static yo hini(en v, u64 n) {
 yo a;
 bind(a, cells(v, n + 2));
 a[n].ll = NULL;
 a[n+1].ll = (vm*) a;
 set64((mem) a, nil, n);
 return a + n; }

CO(mk_yo) {
 yo k;
 bind(k, hini(v, m+1));
 return ee1((terp*)(e ? name(*e) : nil), k); }

static obj apply(lips, obj, obj) NoInline;
ob eval(en v, ob x) {
  ob args;
  bind(args, pair(v, x, nil));
  return apply(v, homp(Eva) ? Eva : tbl_get(v, Top, Eva), args); }

// return to C
Vm(yield) { Pack(); return xp; }
static NoInline ob apply(en v, ob f, ob x) {
  Push(f, x);
  yo h;
  bind(h, cells(v, 5));
  h[0].ll = (vm*) call;
  h[1].ll = (vm*) _N(2);
  h[2].ll = (vm*) yield;
  h[3].ll = NULL;
  h[4].ll = (vm*) h;
  f = (ob) h, x = tbl_get(v, Top, App);
  return call(v, f, v->fp, v->sp, v->hp, x); }

// instructions used by the compiler
Vm(hom_u) {
  ob x;
  Arity(1);
  TypeCheck(x = *Argv, Num);
  i64 len = N(x) + 2;
  Have(len);
  yo h = (yo) hp;
  hp += len;
  set64((mem) h, nil, len);
  h[len-1].ll = (vm*) h;
  h[len-2].ll = NULL;
  Go(ret, (ob) (h+len-2)); }

Vm(hfin_u) {
  Arity(1);
  obj a = *Argv;
  Tc(a, Hom);
  button(H(a))[1].ll = (vm*) a;
  Go(ret, a); }

Vm(emx) { yo h = (yo) *sp++ - 1; h->ll = (vm*) xp;    Ap(ip+word, (ob) h); }
Vm(emi) { yo h = (yo) *sp++ - 1; h->ll = (vm*) N(xp); Ap(ip+word, (ob) h); }

Vm(emx_u) {
 Arity(2);
 ob h = Argv[1];
 CheckType(h, Hom);
 h -= word;
 H(h)->ll = (vm*) Argv[0];
 Go(ret, h); }

Vm(emi_u) {
 Ary(2);
 Tc(Argv[0], Num);
 obj h = Argv[1];
 Tc(h, Hom);
 h -= word;
 H(h)->ll = (vm*) N(Argv[0]);
 Go(ret, h); }

Vm(hgeti_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  Go(ret, Put(H(*Argv)->ll)); }
Vm(hgetx_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  Go(ret, (obj) H(*Argv)->ll); }

Vm(hseek_u) {
  Ary(2);
  Tc(Argv[0], Hom);
  Tc(Argv[1], Num);
  Go(ret, _H(H(Argv[0]) + N(Argv[1]))); }

ob analyze(en v, ob x) {
  with(x, Push(Put(em_i), Put(ret), Put(mk_yo)));
  return (ob) xpn_yo(v, NULL, 0, x); }

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

static Vm(clos) { Clos = (ob) H(ip)[1].ll; Ap((obj) H(ip)[2].ll, xp); }
// finalize function instance closure
static Vm(clos1) { H(ip)->ll = (vm*) clos; H(ip)[1].ll = (vm*) xp; Next(0); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static Vm(clos0) {
 ob ec  = (ob) H(ip)[1].ll,
    arg = V(ec)->xs[0],
    loc = V(ec)->xs[1];
 u64 adic = nilp(arg) ? 0 : V(arg)->len;
 Have(Width(fr) + adic + 1);
 i64 off = (mem) fp - sp;
 H(ip)->ll = (vm*) clos1;
 sp -= adic;
 cpy64(sp, V(arg)->xs, adic);
 ec = (ob) H(ip)[1].ll;
 fp = sp -= Width(fr);
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
  ob x = (ob) H(ip)[1].ll, arg = nil;
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
  yo at = (yo) (block+6); // compiler thread (1 instruction 2 data 2 tag)

  t->len = 5; // initialize alpha closure
  t->xs[0] = arg;
  t->xs[1] = xp; // Locs or nil
  t->xs[2] = Clos;
  t->xs[3] = B(x);
  t->xs[4] = (ob) at;

  at[0].ll = (vm*) clos0;
  at[1].ll = (vm*) putvec(t);
  at[2].ll = (vm*) A(x);
  at[3].ll = NULL;
  at[4].ll = (vm*) at;

  Ap(ip + word * 2, (ob) at); }

Vm(encll) { Go(encl, Locs); }
Vm(encln) { Go(encl, nil); }

NoInline ob homnom(en v, ob x) {
  terp *k = (terp*) H(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return homnom(v, (ob) H(x)[2].ll);
  mem h = (mem) H(x);
  while (*h) h++;
  x = h[-1];
  int inb = (mem) x >= v->pool && (mem) x < v->pool+v->len;
  return inb ? x : nil; }

Vm(hnom_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  xp = homnom(v, *Argv);
  Jump(ret); }

ob sequence(en v, ob a, ob b) {
  yo h;
  with(a, with(b, h = cells(v, 8)));
  bind(h, h);
  h[0].ll = (vm*) imm;
  h[1].ll = (vm*) a;
  h[2].ll = (vm*) call;
  h[3].ll = (vm*) _N(0);
  h[4].ll = (vm*) jump;
  h[5].ll = (vm*) b;
  h[6].ll = NULL;
  h[7].ll = (vm*) h;
  return (ob) h; }
