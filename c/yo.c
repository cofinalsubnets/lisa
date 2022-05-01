#include "lips.h"
#include "ll.h"

////
/// host embedding
//
// FIXME these names may be confusing
// Pull "pulls back the pushout" (pushes on the stack)
// Push "pushes out the pullback" (consumes the stack)
#define Pull(...) pushs(v, __VA_ARGS__, (ob) 0)
#define Push(m) ((c1*)getnum(*v->sp++))(v,e,m)

static bool pushss(em v, i64 i, va_list xs) {
  bool _;
  ob x = va_arg(xs, ob);
  if (!x) return Avail >= i || please(v, i);
  with(x, _ = pushss(v, i+1, xs));
  bind(_, _);
  *--v->sp = x;
  return _; }

static bool pushs(em v, ...) {
  va_list xs;
  va_start(xs, v);
  bool _ = pushss(v, 0, xs);
  va_end(xs);
  return _; }

// emit code backwards like cons
static Inline yo ee1(ll *i, yo k) {
  return (--k)->ll = i, k; }
static Inline yo ee2(ll *i, ob x, yo k) {
  return ee1(i, ee1((ll*) x, k)); }


// FIXME please stop using v->{x,i}p as aux stacks for storing
// code entry points when generating conditionals
//
// this phase does no optimization

// " compilation environments "
typedef struct { ob arg, loc, clo, par, nom, sig; } *ce;
// the current lexical environment is passed to compiler
// functions as a pointer to an object, either a tuple with a
// structure specified below, or nil for toplevel. it's a
// pointer to an object, instead of just an object, so it can
// be gc-protected once instead of separately by every function.
// in the other compiler it's just a regular object.
#define arg(x)  ((ce)(x))->arg // argument variables : a list
#define loc(x)  ((ce)(x))->loc // local variables : a list
#define clo(x)  ((ce)(x))->clo // closure variables : a list
#define par(x)  ((ce)(x))->par // surrounding scope : tuple or nil
#define name(x) ((ce)(x))->nom // function name : a symbol or nil
#define asig(x) ((ce)(x))->sig // arity signature : an integer
// for a function f let n be the number of required arguments.
// then if f takes a fixed number of arguments the arity
// signature is n; otherwise it's -n-1.

static bool scan(em, ob*, ob);
typedef yo c1(em, ob*, u64), c2(mo, ob*, u64, ob);
static c1 x_yo_, let_yo_bind, em_i, em_i_d, mk_yo;
static c2 x_yo, var_yo, two_yo, im_yo;
static ob yo_yo_clo(em, ob*, ob, ob),
          yo_yo_lam(mo, ob*, ob, ob);

enum { Here, Loc, Arg, Clo, Wait };
#define Co(nom,...) static yo nom(em v, ob* e, u64 m, ##__VA_ARGS__)
#define CO Co
#define Put(x) putnum((i64)(x))

// helper functions for lists
static i64 lidx(ob l, ob x) {
  for (i64 i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

static ob linitp(em v, ob x, ob* d) {
  ob y;
  if (!twop(B(x))) return *d = x, nil;
  with(x, y = linitp(v, B(x), d));
  bind(y, y);
  return pair(v, A(x), y); }

static ob snoc(em v, ob l, ob x) {
  if (!twop(l)) return pair(v, x, l);
  with(l, x = snoc(v, B(l), x));
  bind(x, x);
  return pair(v, A(l), x); }

static yo ini_yo(em v, u64 n) {
  yo a;
  bind(a, cells(v, n + 2));
  a[n].ll = NULL;
  a[n+1].ll = (ll*) a;
  set64((ob*) a, nil, n);
  return a + n; }

static yo tuplr(st v, u64 i, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return ini_yo(v, i);
  yo k;
  with(x, k = tuplr(v, i+1, xs));
  bind(k, k);
  return ee1((ll*) x, k); }

static ob tupl(em v, ...) {
  yo t;
  va_list xs;
  va_start(xs, v);
  t = tuplr(v, 0, xs);
  va_end(xs);
  return (ob) t; }

static yo imx(em v, ob *e, i64 m, ll *i, ob x) {
  bind(x, Pull(Put(i), x));
  return em_i_d(v, e, m); }

#define Bind(v, x) if(!((v)=(x)))goto fail
static NoInline ob rw_let_fn(em v, ob x) {
  ob y;
  for (mm(&x); twop(A(x));) {
    Bind(y, snoc(v, BA(x), AB(x)));
    Bind(y, pair(v, La, y));
    Bind(y, pair(v, y, BB(x)));
    Bind(x, pair(v, AA(x), y)); }
  return um, x; fail:
  return um, 0; }

static int scan_def(em v, ob *e, ob x) {
  if (!twop(x)) return 1; // this is an even case so export all the definitions to the local scope
  if (!twop(B(x))) return 0; // this is an odd case so ignore these, they'll be imported after the rewrite
  mm(&x);
  int r = scan_def(v, e, BB(x));
  if (r == 1) {
    Bind(x, rw_let_fn(v, x));
    ob y;
    Bind(y, pair(v, A(x), loc(*e)));
    loc(*e) = y;
    Bind(y, scan(v, e, AB(x))); }
  return um, r; fail:
  return um, -1; }

static bool scan(em v, ob* e, ob x) {
  if (!twop(x) || A(x) == La || A(x) == Qt) return true;
  if (A(x) == De) return scan_def(v, e, B(x)) != -1;
  mm(&x);
  for (; twop(x); x = B(x))
    if (!scan(v, e, A(x))) return um, false;
  return um, true; }

static ob asign(em v, ob a, i64 i, ob*m) {
  ob x;
  if (!twop(a)) return *m = i, a;
  if (twop(B(a)) && AB(a) == Va) {
    *m = -i-1;
    return pair(v, A(a), nil); }
  with(a, x = asign(v, B(a), i+1, m));
  bind(x, x);
  return pair(v, A(a), x); }

static Inline ob new_scope(em v, ob*e, ob a, ob n) {
  i64 s = 0;
  with(n, a = asign(v, a, 0, &s));
  bind(a, a);
  return tupl(v, a, nil, nil, e ? *e : nil, n, putnum(s), (ob)0); }

static Inline ob comp_body(em v, ob*e, ob x) {
  bind(x, Pull(Put(x_yo_), x,
               Put(em_i), Put(ret),
               Put(mk_yo)));
  scan(v, e, v->sp[1]);
  bind(x, (ob) Push(4)); // 4 = 2 + 2
  i64 i = llen(loc(*e));
  if (i) x = (ob) ee2(locals, putnum(i), (yo) x);
  i = getnum(asig(*e));
  if (i > 0) x = (ob) ee2(arity, putnum(i), (yo) x);
  else if (i < 0) x = (ob) ee2(vararg, putnum(-i-1), (yo) x);
  button(gethom(x))[1].ll = (ll*) x;
  return twop(clo(*e)) ? pair(v, clo(*e), x) : x; }

// takes a lambda expr, returns either a pair or or a
// hom depending on if the function has free variables or not
// (in the former case the car is the list of free variables
// and the cdr is a hom that assumes the missing variables
// are available in the closure).
static Inline ob yo_yo_lam(em v, ob* e, ob n, ob l) {
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

static Inline ob yo_yo_clo(em v, ob*e, ob arg, ob seq) {
  i64 i = llen(arg);
  mm(&arg), mm(&seq);
  bool _;
  Bind(_, Pull(
    Put(em_i_d), Put(take), putnum(i),
    Put(mk_yo)));
  while (twop(arg)) {
    Bind(_, Pull(
      Put(x_yo_), A(arg),
      Put(em_i), Put(push)));
    arg = B(arg); }

  Bind(arg, (ob) Push(0));
  return um, um, pair(v, seq, arg); fail:
  return um, um, 0; }

CO(yo_yo, ob x) {
 ll* j = imm;
 ob k, nom = *v->sp == Put(let_yo_bind) ? v->sp[1] : nil;
 with(nom, with(x, k = (ob) Push(m+2)));
 bind(k, k);
 mm(&k);
 if (twop(x = yo_yo_lam(v, e, nom, x)))
   j = e && twop(loc(*e)) ? encll : encln,
   x = yo_yo_clo(v, e, A(x), B(x));
 um;
 bind(x, x);
 return ee2(j, x, (yo) k); }

CO(im_yo, ob x) {
  bind(x, Pull(Put(imm), x));
  return em_i_d(v, e, m); }

CO(let_yo_bind) {
  ob y = *v->sp++;
  return e ? imx(v, e, m, loc_, putnum(lidx(loc(*e), y))) :
             imx(v, e, m, tbind, y); }

static bool let_yo_r(em v, ob*e, ob x) {
  bool _ = true;
  if (twop(x)) {
    bind(x, rw_let_fn(v, x));
    with(x, _ = let_yo_r(v, e, BB(x)));
    bind(_, _);
    bind(_, Pull(Put(x_yo_), AB(x), Put(let_yo_bind), A(x))); }
  return _; }

// syntactic sugar for define
static Inline ob def_sug(em v, ob x) {
  ob y = nil;
  with(y, x = linitp(v, x, &y));
  bind(x, x);
  bind(x, pair(v, x, y));
  bind(x, pair(v, Se, x));
  bind(x, pair(v, x, nil));
  bind(x, pair(v, La, x));
  return pair(v, x, nil); }

Co(let_yo, ob x) {
  if (!twop(B(x))) return im_yo(v, e, m, nil);
  if (llen(B(x)) % 2) {
    bind(x, def_sug(v, x));
    return x_yo(v, e, m, x); }
  bind(x, let_yo_r(v, e, B(x)));
  return Push(m); }

// the following functions are "post" or "pre"
// the antecedent/consequent in the sense of
// return order, ie. "pre_con" runs immediately
// before the consequent code is generated.

// before generating anything, store the
// exit address in stack 2
Co(if_yo_pre) {
  ob x;
  bind(x, (ob) Push(m));
  bind(x, pair(v, x, (ob) v->ip));
  v->ip = (yo) x;
  return (yo) A(x); }

// before generating a branch emit a jump to
// the top of stack 2
Co(if_yo_pre_con) {
  yo x, k;
  bind(x, Push(m + 2));
  k = (yo) A((ob) v->ip);
  return k->ll == ret ? ee1(ret, x) : ee2(jump, (ob) k, x); }

// after generating a branch store its address
// in stack 1
Co(if_yo_post_con) {
  ob x;
  bind(x, (ob) Push(m));
  bind(x, pair(v, x, v->xp));
  v->xp = x;
  return (yo) A(x); }

// before generating an antecedent emit a branch to
// the top of stack 1
Co(if_yo_pre_ant) {
  yo x;
  bind(x, Push(m+2));
  x = ee2(branch, A(v->xp), x);
  v->xp = B(v->xp);
  return x; }

static bool if_yo_loop(em v, ob*e, ob x) {
  bool _;
  if (!twop(x)) bind(x, pair(v, nil, nil));
  if (!twop(B(x)))
    return Pull(Put(x_yo_), A(x), Put(if_yo_pre_con));
  with(x,
    _ = Pull(
      Put(if_yo_post_con),
      Put(x_yo_), AB(x),
      Put(if_yo_pre_con)));
  bind(_, _);
  with(x, _ = if_yo_loop(v, e, BB(x)));
  bind(_, _);
  return Pull(
    Put(x_yo_), A(x),
    Put(if_yo_pre_ant)); }

CO(if_yo, ob x) {
  bool _;
  with(x, _ = Pull(Put(if_yo_pre)));
  bind(_, _);
  bind(_, if_yo_loop(v, e, B(x)));
  yo k;
  bind(k, Push(m));
  v->ip = (yo) B((ob) v->ip);
  return k; }

CO(em_call) {
  ob a = *v->sp++;
  yo k;
  bind(k, Push(m + 2));
  return ee2(k->ll == ret ? rec : call, a, k); }

static ob lookup_mod(em v, ob x) {
  return tbl_get(v, Top, x); }

static ob lookup_lex(em v, ob e, ob y) {
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
      with(x, y = (ob) Push(m+2));
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

CO(x_yo_) { return x_yo(v, e, m, *v->sp++); }
CO(x_yo, ob x) { return (symp(x) ? var_yo :
                           twop(x) ? two_yo :
                                     im_yo)(v, e, m, x); }

CO(ap_yo, ob fun, ob args) {
  mm(&args);
  Bind(fun, Pull(
    Put(x_yo_), fun,
    Put(em_i), Put(idH),
    Put(em_call), putnum(llen(args))));
  while (twop(args)) {
    Bind(fun, Pull(
      Put(x_yo_), A(args),
      Put(em_i), Put(push)));
    args = B(args); }

  return um, Push(m); fail:
  return um, NULL; }

static bool seq_yo_loop(em v, ob*e, ob x) {
  bool _ = true;
  if (twop(x)) {
    with(x, _ = seq_yo_loop(v, e, B(x)));
    bind(_, _);
    bind(_, Pull(Put(x_yo_), A(x))); }
  return _; }

CO(two_yo, ob x) {
  ob z = A(x);
  if (z == If) return if_yo(v, e, m, x);
  if (z == De) return let_yo(v, e, m, x);
  if (z == La) return yo_yo(v, e, m, x);
  if (z == Se) {
    if (!twop(x = B(x))) bind(x, pair(v, x, nil));
    bind(x, seq_yo_loop(v, e, x));
    return Push(m); }
  if (z == Qt) {
    x = twop(x = B(x)) ? A(x) : x;
    return im_yo(v, e, m, x); }
  return ap_yo(v, e, m, A(x), B(x)); }

CO(em_i) {
  ll* i = (ll*) getnum(*v->sp++);
  yo k;
  bind(k, Push(m+1));
  return ee1(i, k); }

CO(em_i_d) {
  ll* i = (ll*) getnum(*v->sp++);
  ob x = *v->sp++;
  yo k;
  with(x, k = Push(m+2));
  bind(k, k);
  return ee2(i, x, k); }

CO(mk_yo) {
  yo k;
  bind(k, ini_yo(v, m+1));
  return ee1((ll*)(e ? name(*e) : nil), k); }

static ob apply(em, ob, ob) NoInline;
ob eval(em v, ob x) {
  ob args;
  bind(args, pair(v, x, nil));
  return apply(v, homp(Eva) ? Eva : tbl_get(v, Top, Eva), args); }

// return to C
Ll(yield) { Pack(); return xp; }
static NoInline ob apply(em v, ob f, ob x) {
  Pull(f, x);
  yo h;
  bind(h, cells(v, 5));
  h[0].ll = call;
  h[1].ll = (ll*) putnum(2);
  h[2].ll = yield;
  h[3].ll = NULL;
  h[4].ll = (ll*) h;
  x = tbl_get(v, Top, App);
  return call(v, (ob) h, (ob*) v->fp, v->sp, v->hp, x); }

// instructions used by the compiler
Ll(hom_u) {
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

Ll(hfin_u) {
  Arity(1);
  ob a = *Argv;
  TypeCheck(a, Hom);
  button(gethom(a))[1].ll = (ll*) a;
  return ApC(ret, a); }

Ll(emx) {
  yo h = (yo) *sp++ - 1;
  h->ll = (ll*) xp;
  return ApN(1, (ob) h); }

Ll(emi) {
  yo h = (yo) *sp++ - 1;
  h->ll = (ll*) getnum(xp);
  return ApN(1, (ob) h); }

Ll(emx_u) {
 Arity(2);
 ob h = Argv[1];
 CheckType(h, Hom);
 h -= sizeof(yo);
 gethom(h)->ll = (ll*) Argv[0];
 return ApC(ret, h); }

Ll(emi_u) {
 Arity(2);
 TypeCheck(Argv[0], Num);
 ob h = Argv[1];
 TypeCheck(h, Hom);
 h -= sizeof(void*);
 gethom(h)->ll = (ll*) getnum(Argv[0]);
 return ApC(ret, h); }

Ll(hgeti_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, Put(gethom(*Argv)->ll)); }

Ll(hgetx_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, (ob) gethom(*Argv)->ll); }

Ll(hseek_u) {
  Arity(2);
  TypeCheck(Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  return ApC(ret, puthom(gethom(Argv[0]) + getnum(Argv[1]))); }

ob analyze(em v, ob x) {
  with(x, Pull(Put(em_i), Put(ret), Put(mk_yo)));
  return (ob) x_yo(v, NULL, 0, x); }

Ll(ev_u) {
  Arity(1);
  if (homp(Eva)) ip = Eva;
  else { Pack();
         bind(v->ip, (yo) analyze(v, *Argv));
         Unpack(); }
  return ApY(ip, xp); }

Ll(bootstrap) {
  Arity(1);
  xp = *Argv;
  TypeCheck(xp, Hom);
  // neither intern nor tbl_set will allocate if ev is already interned / defined
  tbl_set(v, Top, interns(v, "ev"), Eva = xp);
  return ApC(ret, xp); }

static Ll(clos) {
  Clos = (ob) IP[1].ll;
  return ApY((ob) IP[2].ll, xp); }
// finalize function instance closure
static Ll(clos1) {
  IP->ll = clos;
  IP[1].ll = (ll*) xp;
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
  ob *ec  = (ob*) IP[1].ll,
     arg = ec[0],
     loc = ec[1];
  u64 adic = nilp(arg) ? 0 : getnum(*(ob*)arg);
  Have(Width(fr) + adic + 1);
  i64 off = (ob*) fp - sp;
  IP->ll = clos1;
  sp -= adic;
  cpy64(sp, (ob*) arg + 1, adic);
  ec = (ob*) IP[1].ll;
  fp = (void*) (sp -= Width(fr));
  Retp = ip;
  Subr = putnum(off);
  Argc = putnum(adic);
  Clos = ec[2];
  if (!nilp(loc)) *--sp = loc;
  return ApY(ec[3], xp); }

// the next few functions create and store
// lexical environments.
static Vm(encl) {
  i64 n = getnum(Argc);
  n += n ? 14 : 11;
  Have(n);
  ob x = (ob) IP[1].ll, arg = nil;
  ob* block = hp;
  hp += n;
  if (n > 11) {
    n -= 11;
    ob *t = block;
    block += n;
    for (t[n-2] = 0,
         t[n-1] = (ob) t,
         t[0] = putnum(n-=3);
         n--;
         t[n+1] = Argv[n]);
    arg = (ob) t; }

  yo t = (yo) block, // compiler thread closure array (1 length 5 elements)
     at = t + 6; // compiler thread (1 instruction 2 data 2 tag)

  t[0].ll = (ll*) arg;
  t[1].ll = (ll*) xp; // Locs or nil
  t[2].ll = (ll*) Clos;
  t[3].ll = (ll*) B(x);
  t[4].ll = NULL;
  t[5].ll = (ll*) t;

  at[0].ll = clos0;
  at[1].ll = (ll*) t;
  at[2].ll = (ll*) A(x);
  at[3].ll = NULL;
  at[4].ll = (ll*) at;

  return ApN(2, (ob) at); }

Ll(encll) { return ApC(encl, Locs); }
Ll(encln) { return ApC(encl, nil); }

NoInline ob homnom(em v, ob x) {
  ll *k = (ll*) gethom(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return homnom(v, (ob) gethom(x)[2].ll);
  ob* h = (ob*) gethom(x);
  while (*h) h++;
  x = h[-1];
  int inb = (ob*) x >= v->pool && (ob*) x < v->pool+v->len;
  return inb ? x : nil; }

Ll(hnom_u) {
  Arity(1);
  TypeCheck(*Argv, Hom);
  return ApC(ret, homnom(v, *Argv)); }

ob sequence(em v, ob a, ob b) {
  yo h;
  with(a, with(b, h = cells(v, 8)));
  bind(h, h);
  h[0].ll = imm;
  h[1].ll = (ll*) a;
  h[2].ll = call;
  h[3].ll = (ll*) N0;
  h[4].ll = jump;
  h[5].ll = (ll*) b;
  h[6].ll = NULL;
  h[7].ll = (ll*) h;
  return (ob) h; }
