#include "lips.h"
#include "ll.h"

////
/// bootstrap thread compiler
//
// passes continuations by pushing function pointers
// onto the main stack with Push and calling them with Pull
#define Push(...) pushs(v, __VA_ARGS__, (ob) 0)
#define Pull(m) ((c1*)getnum(*v->sp++))(v,e,m)
// also uses Xp and Ip as stacks for storing code entry points
// when generating conditionals.
//
// this compiler emits runtime type checks for safety but does
// minimal optimization and analysis since all it has to do
// is bootstrap the main compiler.


// " compilation environments "
typedef struct { ob arg, loc, clo, par, nom, sig; } *cenv;
#define Ce(x) ((cenv)(getvec(x)->xs))
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

static u1 scan(en, ob*, ob);
static ob yo_yo_clo(en, ob*, ob, ob), ltu(en, ob*, ob, ob);
typedef yo c1(en, ob*, u64), c2(en, ob*, u64, ob);
static c1 xpn_yo_, let_yo_bind, em_i, em_i_d, mk_yo;
static c2 xpn_yo, var_yo, form_yo, im_yo;

enum { Here, Loc, Arg, Clo, Wait };
#define CO(nom,...) static yo nom(en v, ob* e, u64 m, ##__VA_ARGS__)
#define Put(x) putnum((i64)(x))

// helper functions for lists
static i64 lidx(ob l, ob x) {
  for (i64 i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

static ob linitp(en v, ob x, ob* d) {
  ob y;
  if (!twop(B(x))) return *d = x, nil;
  with(x, y = linitp(v, B(x), d));
  bind(y, y);
  return pair(v, A(x), y); }

static ob snoc(en v, ob l, ob x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = snoc(v, B(l), x));
  bind(x, x);
  return pair(v, A(l), x); }

static u1 pushss(en v, i64 i, va_list xs) {
  u1 _;
  ob x = va_arg(xs, ob);
  if (!x) return Avail >= i || please(v, i);
  with(x, _ = pushss(v, i+1, xs));
  bind(_, _);
  *--v->sp = x;
  return _; }

static u1 pushs(en v, ...) {
  va_list xs; u1 _;
  va_start(xs, v);
  _ = pushss(v, 0, xs);
  va_end(xs);
  return _; }

static vec tuplr(en v, i64 i, va_list xs) {
  vec t;
  ob x = va_arg(xs, ob);
  if (!x) {
    bind(t, cells(v, Width(vec) + i));
    t->len = i; }
  else {
    with(x, t = tuplr(v, i+1, xs));
    bind(t, t);
    t->xs[i] = x; }
  return t; }

static ob tupl(en v, ...) {
 va_list xs; vec t;
 va_start(xs, v), t = tuplr(v, 0, xs), va_end(xs);
 bind(t, t);
 return putvec(t); }

// emit code backwards like cons
SI yo em1(vm *i, yo k) { return (--k)->ll = i, k; }
SI yo em2(vm *i, ob j, yo k) { return em1(i, em1((vm*)j, k)); }
SI yo ee1(vm *i, yo k) { return (--k)->ll = (vm*) i, k; }
SI yo ee2(vm *i, ob x, yo k) { return ee1(i, ee1((vm*) x, k)); }

static yo imx(en v, ob *e, i64 m, vm *i, ob x) {
  bind(x, Push(Put(i), x));
  return em_i_d(v, e, m); }

#define Bind(v, x) if(!((v)=(x)))goto fail
SNI ob rw_let_fn(en v, ob x) {
  ob y;
  for (mm(&x); twop(A(x));) {
    Bind(y, snoc(v, BA(x), AB(x)));
    Bind(y, pair(v, La, y));
    Bind(y, pair(v, y, BB(x)));
    Bind(x, pair(v, AA(x), y)); }
  return um, x; fail:
  return um, 0; }

static i1 scan_def(en v, ob *e, ob x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  mm(&x);
  u1 r = scan_def(v, e, BB(x));
  if (r == 1) {
    Bind(x, rw_let_fn(v, x));
    ob y;
    Bind(y, pair(v, A(x), loc(*e)));
    loc(*e) = y;
    Bind(y, scan(v, e, AB(x))); }
  return um, r; fail:
  return um, -1; }

static u1 scan(en v, ob* e, ob x) {
  if (!twop(x) || A(x) == La || A(x) == Qt) return true;
  if (A(x) == De) return scan_def(v, e, B(x)) != -1;
  mm(&x);
  for (; twop(x); x = B(x))
    if (!scan(v, e, A(x))) return um, false;
  return um, true; }

static ob asign(en v, ob a, i64 i, ob*m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == Va) {
    *m = -i-1;
    return pair(v, A(a), nil); }
  with(a, x = asign(v, B(a), i+1, m));
  bind(x, x);
  return pair(v, A(a), x); }

SI ob new_scope(en v, ob*e, ob a, ob n) {
  i64 s = 0;
  with(n, a = asign(v, a, 0, &s));
  bind(a, a);
  return tupl(v, a, nil, nil, e ? *e : nil, n, putnum(s), (ob)0); }

SI ob comp_body(en v, ob*e, ob x) {
  bind(x, Push(Put(xpn_yo_), x,
               Put(em_i), Put(ret),
               Put(mk_yo)));
  scan(v, e, v->sp[1]);
  bind(x, (ob) Pull(4)); // 4 = 2 + 2
  i64 i = llen(loc(*e));
  if (i) x = (ob) em2(locals, putnum(i), (yo) x);
  i = getnum(asig(*e));
  if (i > 0) x = (ob) em2(arity, putnum(i), (yo) x);
  else if (i < 0) x = (ob) em2(vararg, putnum(-i-1), (yo) x);
  button(gethom(x))[1].ll = (vm*) x;
  return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
SI ob ltu(en v, ob* e, ob n, ob l) {
  ob y = nil;
  l = B(l);
  mm(&n); mm(&y); mm(&l);
  Bind(l, twop(l) ? l : pair(v, l, nil));
  Bind(l, linitp(v, l, &y));
  Bind(n, pair(v, n, e ? name(*e) : nil));
  Bind(n, new_scope(v, e, l, n));
  Bind(l, comp_body(v, &n, A(y)));
  return um, um, um, l; fail:
  return um, um, um, 0; }

SI ob yo_yo_clo(en v, ob*e, ob arg, ob seq) {
  i64 i = llen(arg);
  mm(&arg), mm(&seq);
  u1 _;
  Bind(_, Push(
    Put(em_i_d), Put(take), putnum(i),
    Put(mk_yo)));
  while (twop(arg)) {
    Bind(_, Push(
      Put(xpn_yo_), A(arg),
      Put(em_i), Put(push)));
    arg = B(arg); }

  Bind(arg, (ob) Pull(0));
  return um, um, pair(v, seq, arg); fail:
  return um, um, 0; }

CO(yo_yo, ob x) {
 vm* j = imm;
 ob k, nom = *v->sp == Put(let_yo_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = (ob) Pull(m+2)));
 bind(k, k);
 mm(&k);
 if (twop(x = ltu(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = yo_yo_clo(v, e, A(x), B(x));
 um;
 bind(x, x);
 return ee2(j, x, (yo) k); }

CO(im_yo, ob x) {
  bind(x, Push(Put(imm), x));
  return em_i_d(v, e, m); }

CO(let_yo_bind) {
  ob y = *v->sp++;
  return e ? imx(v, e, m, loc_, putnum(lidx(loc(*e), y))) :
             imx(v, e, m, tbind, y); }

static u1 let_yo_r(en v, ob*e, ob x) {
  u1 _ = true;
  if (twop(x)) {
    bind(x, rw_let_fn(v, x));
    with(x, _ = let_yo_r(v, e, BB(x)));
    bind(_, _);
    bind(_, Push(Put(xpn_yo_), AB(x), Put(let_yo_bind), A(x))); }
  return _; }

// syntactic sugar for define
SI ob def_sug(en v, ob x) {
  ob y = nil;
  with(y, x = linitp(v, x, &y));
  bind(x, x);
  bind(x, pair(v, x, y));
  bind(x, pair(v, Se, x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, La, x));
  return pair(v, x, nil); }

CO(let_yo, ob x) {
  if (!twop(B(x))) return im_yo(v, e, m, nil);
  if (llen(B(x)) % 2) {
    bind(x, def_sug(v, x));
    return xpn_yo(v, e, m, x); }
  bind(x, let_yo_r(v, e, B(x)));
  return Pull(m); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
CO(if_yo_pre) {
  ob x;
  bind(x, (ob) Pull(m));
  bind(x, pair(v, x, (ob) v->ip));
  v->ip = (yo) x;
  return (yo) A(x); }

// before generating a branch emit a jump to
// the top of stack 2
CO(if_yo_pre_con) {
  yo x, k;
  bind(x, Pull(m + 2));
  k = (yo) A((ob) v->ip);
  return k->ll == (vm*) ret ? ee1(ret, x) : ee2(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
CO(if_yo_post_con) {
  ob x;
  bind(x, (ob) Pull(m));
  bind(x, pair(v, x, v->xp));
  v->xp = x;
  return (yo) A(x); }

// before generating an antecedent emit a branch to
// the top of stack 1
CO(if_yo_pre_ant) {
  yo x;
  bind(x, Pull(m+2));
  x = ee2(branch, A(v->xp), x);
  v->xp = B(v->xp);
  return x; }

static u1 if_yo_loop(en v, ob*e, ob x) {
  u1 _;
  if (!twop(x)) bind(x, pair(v, nil, nil));
  if (!twop(B(x)))
    return Push(Put(xpn_yo_), A(x), Put(if_yo_pre_con));
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

CO(if_yo, ob x) {
  u1 _;
  with(x, _ = Push(Put(if_yo_pre)));
  bind(_, _);
  bind(_, if_yo_loop(v, e, B(x)));
  yo k;
  bind(k, Pull(m));
  v->ip = (yo) B((ob) v->ip);
  return k; }

CO(em_call) {
  ob a = *v->sp++;
  yo k;
  bind(k, Pull(m + 2));
  return ee2(k->ll == (vm*) ret ? rec : call, a, k); }

static ob lookup_mod(en v, ob x) {
  return tbl_get(v, Top, x); }

static ob lookup_lex(en v, ob e, ob y) {
  if (nilp(e)) {
    ob q = lookup_mod(v, y);
    return q ? pair(v, putnum(Here), q) : pair(v, putnum(Wait), Top); }
  return
    lidx(loc(e), y) > -1 ? pair(v, putnum(Loc), e) :
    lidx(arg(e), y) > -1 ? pair(v, putnum(Arg), e) :
    lidx(clo(e), y) > -1 ? pair(v, putnum(Clo), e) :
    lookup_lex(v, par(e), y); }

CO(var_yo, ob x) {
  ob y, q;
  with(x, q = lookup_lex(v, e ? *e:nil, x));
  bind(q, q);
  y = A(q);
  switch (getnum(y)) {
    case Here: return im_yo(v, e, m, B(q));
    case Wait:
      bind(x, pair(v, B(q), x));
      with(x, y = (ob) Pull(m+2));
      bind(y, y);
      with(y, x = pair(v, putnum(sizeof(ob)), x));
      bind(x, x);
      return ee2(lbind, x, (yo) y);
    default:
      if (B(q) == *e) switch (getnum(y)) {
        case Loc:
          return imx(v, e, m, loc, putnum(lidx(loc(*e), x)));
        case Arg:
          return imx(v, e, m, arg, putnum(lidx(arg(*e), x)));
        default:
          return imx(v, e, m, clo, putnum(lidx(clo(*e), x))); }
      y = llen(clo(*e));
      with(x, q = snoc(v, clo(*e), x));
      bind(q, q);
      clo(*e) = q;
      return imx(v, e, m, clo, putnum(y)); } }

CO(xpn_yo_) { return xpn_yo(v, e, m, *v->sp++); }
CO(xpn_yo, ob x) { return (symp(x) ? var_yo :
                           twop(x) ? form_yo :
                                     im_yo)(v, e, m, x); }

CO(ap_yo, ob fun, ob args) {
  mm(&args);
  Bind(fun, Push(
    Put(xpn_yo_), fun,
    Put(em_i), Put(idH),
    Put(em_call), putnum(llen(args))));
  while (twop(args)) {
    Bind(fun, Push(
      Put(xpn_yo_), A(args),
      Put(em_i), Put(push)));
    args = B(args); }

  return um, Pull(m); fail:
  return um, NULL; }

static u1 seq_yo_loop(en v, ob*e, ob x) {
  u1 _ = true;
  if (twop(x)) {
    with(x, _ = seq_yo_loop(v, e, B(x)));
    bind(_, _);
    bind(_, Push(Put(xpn_yo_), A(x))); }
  return _; }

CO(form_yo, ob x) {
  ob z = A(x);
  if (z == If) return if_yo(v, e, m, x);
  if (z == De) return let_yo(v, e, m, x);
  if (z == La) return yo_yo(v, e, m, x);
  if (z == Se) {
    if (!twop(x = B(x))) bind(x, pair(v, x, nil));
    bind(x, seq_yo_loop(v, e, x));
    return Pull(m); }
  if (z == Qt) {
    x = twop(x = B(x)) ? A(x) : x;
    return im_yo(v, e, m, x); }
  return ap_yo(v, e, m, A(x), B(x)); }

CO(em_i) {
  vm* i = (vm*) getnum(*v->sp++);
  yo k;
  bind(k, Pull(m+1));
  return ee1(i, k); }

CO(em_i_d) {
  vm* i = (vm*) getnum(*v->sp++);
  ob x = *v->sp++;
  yo k;
  with(x, k = Pull(m+2));
  bind(k, k);
  return ee2(i, x, k); }

static yo hini(en v, u64 n) {
  yo a;
  bind(a, cells(v, n + 2));
  a[n].ll = NULL;
  a[n+1].ll = (vm*) a;
  set64((ob*) a, nil, n);
  return a + n; }

CO(mk_yo) {
  yo k;
  bind(k, hini(v, m+1));
  return ee1((vm*)(e ? name(*e) : nil), k); }

static ob apply(en, ob, ob) NoInline;
ob eval(en v, ob x) {
  ob args;
  bind(args, pair(v, x, nil));
  return apply(v, homp(Eva) ? Eva : tbl_get(v, Top, Eva), args); }

// return to C
Vm(yield) { Pack(); return xp; }
SNI ob apply(en v, ob f, ob x) {
  Push(f, x);
  yo h;
  bind(h, cells(v, 5));
  h[0].ll = call;
  h[1].ll = (vm*) putnum(2);
  h[2].ll = yield;
  h[3].ll = NULL;
  h[4].ll = (vm*) h;
  x = tbl_get(v, Top, App);
  return call(v, (ob) h, (ob*) v->fp, v->sp, v->hp, x); }

// instructions used by the compiler
Vm(hom_u) {
  ob x;
  Arity(1);
  TypeCheck(x = *Argv, Num);
  i64 len = getnum(x) + 2;
  Have(len);
  yo h = (yo) hp;
  hp += len;
  set64((ob*) h, nil, len);
  h[len-1].ll = (ll*) h;
  h[len-2].ll = NULL;
  return ApC(ret, (ob) (h+len-2)); }

Vm(hfin_u) {
  Arity(1);
  ob a = *Argv;
  TypeCheck(a, Hom);
  button(gethom(a))[1].ll = (ll*) a;
  return ApC(ret, a); }

Vm(emx) {
  yo h = (yo) *sp++ - 1;
  h->ll = (ll*) xp;
  return ApN(1, (ob) h); }

Vm(emi) {
  yo h = (yo) *sp++ - 1;
  h->ll = (ll*) getnum(xp);
  return ApN(1, (ob) h); }

Vm(emx_u) {
 Arity(2);
 ob h = Argv[1];
 CheckType(h, Hom);
 h -= sizeof(yo);
 gethom(h)->ll = (ll*) Argv[0];
 return ApC(ret, h); }

Vm(emi_u) {
 Ary(2);
 Tc(Argv[0], Num);
 ob h = Argv[1];
 Tc(h, Hom);
 h -= sizeof(void*);
 gethom(h)->ll = (vm*) getnum(Argv[0]);
 return ApC(ret, h); }

Vm(hgeti_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, Put(gethom(*Argv)->ll)); }

Vm(hgetx_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, (ob) gethom(*Argv)->ll); }

Vm(hseek_u) {
  Ary(2);
  Tc(Argv[0], Hom);
  Tc(Argv[1], Num);
  return ApC(ret, puthom(gethom(Argv[0]) + getnum(Argv[1]))); }

ob analyze(en v, ob x) {
  with(x, Push(Put(em_i), Put(ret), Put(mk_yo)));
  return (ob) xpn_yo(v, NULL, 0, x); }

Vm(ev_u) {
  Arity(1);
  if (homp(Eva)) ip = Eva;
  else { Pack();
         bind(v->ip, (yo) analyze(v, *Argv));
         Unpack(); }
  return ApY(ip, xp); }

Vm(bootstrap) {
  Arity(1);
  xp = *Argv;
  TypeCheck(xp, Hom);
  // neither intern nor tbl_set will allocate if ev is already interned / defined
  tbl_set(v, Top, interns(v, "ev"), Eva = xp);
  return ApC(ret, xp); }

static Vm(clos) {
  Clos = (ob) IP[1].ll;
  return ApY((ob) IP[2].ll, xp); }
// finalize function instance closure
static Vm(clos1) {
  IP->ll = (vm*) clos;
  IP[1].ll = (vm*) xp;
  return ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static Vm(clos0) {
  ob ec  = (ob) IP[1].ll,
     arg = getvec(ec)->xs[0],
     loc = getvec(ec)->xs[1];
  u64 adic = nilp(arg) ? 0 : getvec(arg)->len;
  Have(Width(fr) + adic + 1);
  i64 off = (ob*) fp - sp;
  IP->ll = clos1;
  sp -= adic;
  cpy64(sp, getvec(arg)->xs, adic);
  ec = (ob) IP[1].ll;
  fp = (void*) (sp -= Width(fr));
  Retp = ip;
  Subr = putnum(off);
  Argc = putnum(adic);
  Clos = getvec(ec)->xs[2];
  if (!nilp(loc)) *--sp = loc;
  return ApY(getvec(ec)->xs[3], xp); }

// the next few functions create and store
// lexical environments.
static Vm(encl) {
  i64 n = getnum(Argc);
  n += n ? 12 : 11;
  Have(n);
  ob x = (ob) IP[1].ll, arg = nil;
  ob* block = hp;
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

  at[0].ll = clos0;
  at[1].ll = (vm*) putvec(t);
  at[2].ll = (vm*) A(x);
  at[3].ll = NULL;
  at[4].ll = (vm*) at;

  return ApN(2, (ob) at); }

Vm(encll) { return ApC(encl, Locs); }
Vm(encln) { return ApC(encl, nil); }

NoInline ob homnom(en v, ob x) {
  vm *k = (vm*) gethom(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return homnom(v, (ob) gethom(x)[2].ll);
  ob* h = (ob*) gethom(x);
  while (*h) h++;
  x = h[-1];
  int inb = (ob*) x >= v->pool && (ob*) x < v->pool+v->len;
  return inb ? x : nil; }

Vm(hnom_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, homnom(v, *Argv)); }

ob sequence(en v, ob a, ob b) {
  yo h;
  with(a, with(b, h = cells(v, 8)));
  bind(h, h);
  h[0].ll = imm;
  h[1].ll = (vm*) a;
  h[2].ll = call;
  h[3].ll = (vm*) N0;
  h[4].ll = jump;
  h[5].ll = (vm*) b;
  h[6].ll = NULL;
  h[7].ll = (vm*) h;
  return (ob) h; }
