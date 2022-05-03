#include <stdarg.h>
#include "em.h"

#define N1 putnum(1)

// " the virtual machine "
// It's a stack machine with one free register that runs on
// top of the C compiler's calling convention. This lets us
// keep the main state variables in CPU registers without
// needing to write any platform specific code, as long as
// the compiler passes enough arguments in registers and
// optimizes tail calls.

// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function

// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.

Ll(ap_u) {
  Arity(2);
  ob x = Argv[0], y = Argv[1];
  TypeCheck(x, Hom);
  uintptr_t adic = llen(y);
  Have(adic);
  ob off = Subr, rp = Retp;
  sp = Argv + getnum(Argc) - adic;
  for (uintptr_t j = 0; j < adic;)
    sp[j++] = gettwo(y)->a,
    y = gettwo(y)->b;
  return
    fp = (void*) ((fr) sp - 1),
    sp = (ob*) fp,
    Retp = rp,
    Argc = putnum(adic),
    Subr = off,
    Clos = nil,
    ApY(x, nil); }

Ll(vararg) {
  intptr_t reqd = getnum((ob) ip[1].ll),
           vdic = getnum(Argc) - reqd;
  Arity(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1();
    return
      cpyw(fp-1, fp, Width(fr) + getnum(Argc)),
      fp = (void*) ((ob*) fp - 1),
      sp = (ob*) fp,
      Argc += sizeof(ob),
      Argv[reqd] = nil,
      ApN(2, xp); }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (uintptr_t i = vdic; i--;
    t[i].a = Argv[reqd + i],
    t[i].b = puttwo(t+i+1));
  return
    t[vdic-1].b = nil,
    Argv[reqd] = puttwo(t),
    ApN(2, xp); }

// type predicates
#define Tp(t)\
  Ll(t##pp) { return ApN(1, (t##p(xp)?N1:nil)); }\
  Ll(t##p_u) {\
    for (ob *xs = Argv, *l = xs + getnum(Argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, N1); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// stack manipulation
Ll(dupl) { Have1(); --sp; sp[0] = sp[1]; return ApN(1, xp); }

static ll recne;

////
/// Branch Instructions
//
// unconditional jump
Ll(jump) { return ApY((ob) ip[1].ll, xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Ll(nom) {\
  return ApY(test ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq,  eql(*sp++, xp), GF, N1, FF, nil)
Br(brne,  eql(*sp++, xp), FF, N1, GF, nil)

Br(brlt,    *sp++ < xp,  GF, xp, FF, nil)
Br(brlt2,   *sp++ < xp,  FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ > xp,  GF, xp, FF, nil)
Br(brgt2,   *sp++ > xp,  FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
#undef Br

// return from a function
Ll(ret) { return
  ip = (yo) Retp,
  sp = (ob*) ((ob) Argv + Argc - Num),
  fp = (void*) ((ob) sp + Subr - Num),
  ApY(ip, xp); }

// "inner" function call
Ll(call) {
  Have(Width(fr));
  intptr_t adic = getnum((ob) ip[1].ll),
           off = (ob*) fp - (sp + adic);
  return
    fp = (void*) ((fr) sp - 1),
    sp = (ob*) fp,
    Retp = (ob) (ip + 2),
    Subr = putnum(off),
    Clos = nil,
    Argc = putnum(adic),
    ApY(xp, nil); }

// tail call
Ll(rec) { return
  ip = (yo) ip[1].ll,
  Argc != (ob) ip ? ApC(recne, xp) :
  (cpyw(Argv, sp, getnum((ob) ip)),
   sp = (ob*) fp,
   ApY(xp, nil)); }

// tail call with different arity
static Ll(recne) { return
  v->xp = Subr,
  v->ip = (yo) Retp, // save return info
  fp = (void*) (Argv + getnum(Argc - (ob) ip)),
  rcpyw(fp, sp, getnum((ob) ip)), // copy from high to low
  sp = (ob*) (((fr) fp) - 1),
  fp = (void*) sp,
  Retp = (ob) v->ip,
  Argc = (ob) ip,
  Subr = v->xp,
  ApY(xp, Clos = nil); }

// errors
Ll(fail) { return Pack(), err(v, "fail"); }

#define type_err_msg "wrong type : %s for %s"
Ll(type_error) {
  enum class exp = v->xp, act = Q(xp);
  return Pack(), err(v, type_err_msg, tnom(act), tnom(exp)); }

Ll(oob_error) {
  ob a = v->xp, b = (ob) v->ip;
  return Pack(), err(v, "oob : %d >= %d", a, b); }

#define arity_err_msg "wrong arity : %d of %d"
Ll(ary_error) {
  ob a = getnum(Argc), b = v->xp;
  return Pack(), err(v, arity_err_msg, a, b); }

Ll(div_error) { return Pack(), err(v, "/ 0"); }

// type/arity checking
#define DTc(n, t) Ll(n) {\
  if (Q(xp-t)==0) return ApN(1, xp);\
  return v->xp = t, ApC(type_error, xp); }
DTc(idZ, Num) DTc(idH, Hom)
DTc(idT, Tbl) DTc(id2, Two)
Ll(arity) {
  ob reqd = (ob) ip[1].ll;
  return reqd <= Argc ?
    ApN(2, xp) :
    (v->xp = getnum(reqd),
     ApC(ary_error, xp)); }

static Inline void show_call(em v, yo ip, fr fp) {
  fputc('(', stderr);
  emit(v, (ob) ip, stderr);
  for (uintptr_t i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr), emit(v, fp->argv[i++], stderr);
  fputc(')', stderr); }

NoInline ob err(em v, const char *msg, ...) {
  yo ip = (yo) v->ip;
  fr fp = (fr) v->fp,
     top = (fr) (v->pool + v->len);

  // error line
  fputs("# ", stderr);
  if (fp < top) show_call(v, ip, fp), fputs(" : ", stderr);
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  fputc('\n', stderr);

  // backtrace
  if (fp < top) for (;;) {
    ip = (yo) fp->retp,
    fp = (void*) fp + Width(fr) + getnum(fp->argc) + getnum(fp->subd);

    if (fp == top) goto out; else
      fputs("# in ", stderr),
      show_call(v, ip, fp),
      fputc('\n', stderr); } out:

  // reset and yield
  return
    v->sp = v->pool + v->len,
    v->fp = (fr) v->sp,
    v->xp = nil,
    v->ip = (yo) nil,
    0; }


#define mm_u(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);}\
  return ApC(ret, putnum(xp));}

#define mm_void(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);\
    if (x == N0) return ApC(div_error, xp);}\
  return ApC(ret, putnum(xp));}

Ll(sub_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, N0);
  TypeCheck(*Argv, Num);
  if (xp == 1) return ApC(ret, putnum(-getnum(*Argv)));
  mm_u(xp-1,Argv+1,getnum(*Argv),-); }

Ll(sar_u) {
  if (Argc == N0) return ApC(ret, N0);
  TypeCheck(*Argv, Num);
  mm_u(getnum(Argc)-1, Argv+1, getnum(*Argv), >>); }

Ll(sal_u) {
  if (Argc == N0) return ApC(ret, N0);
  TypeCheck(*Argv, Num);
  mm_u(getnum(Argc)-1, Argv+1, getnum(*Argv), <<); }

Ll(dqv) {
  if (xp == N0) return ApC(div_error, xp);
  xp = putnum(getnum(*sp++) / getnum(xp));
  return ApN(1, xp); }

Ll(div_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, N1);
  TypeCheck(*Argv, Num);
  mm_void(xp-1, Argv+1, getnum(*Argv), /); }

Ll(mod) {
  if (xp == N0) return ApC(div_error, xp);
  xp = putnum(getnum(*sp++) % getnum(xp));
  return ApN(1, xp); }

Ll(mod_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, N1);
  TypeCheck(*Argv, Num);
  mm_void(xp-1, Argv+1, getnum(*Argv), %); }

Ll(rnd_u) { return
  xp = putnum(v->rand = lcprng(v->rand)),
  ApC(ret, xp); }

#define OP(nom, x, n) Ll(nom) { xp = (x); return ApN(n, xp); }
#define OP1(nom, x) OP(nom, x, 1)
#define BINOP(nom, xpn) Vm(nom) { xp = (xpn); return ApN(1, xp); }
OP1(neg, putnum(-getnum(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  putnum(getnum(*sp++) * getnum(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  putnum(getnum(*sp++) >> getnum(xp)))
BINOP(sal,  putnum(getnum(*sp++) << getnum(xp)))

#define UBINOP(nom, dflt, op) Ll(nom##_u) { mm_u(getnum(Argc), Argv, dflt, op); }

UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)
#include <string.h>

static NoInline bool eql_(ob a, ob b) { return eql(a, b); }

static NoInline bool eql_two(two a, two b) {
  return eql_(a->a, b->a) && eql_(a->b, b->b); }

static NoInline bool eql_str(str a, str b) {
  return a->len == b->len &&
    strcmp(a->text, b->text) == 0; }

Inline bool eql(ob a, ob b) {
  return a == b || (Q(a) == Q(b) &&
    ((twop(a) && eql_two(gettwo(a), gettwo(b))) ||
     (strp(a) && eql_str(getstr(a), getstr(b))))); }

#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
BINOP(eq, eql(xp, *sp++) ? N1 : nil)
#define cmp(n, op) BINOP(n, op(*sp++, xp) ? xp : nil)
cmp(lt, LT) cmp(lteq, LE) cmp(gteq, GE) cmp(gt, GT)
#undef cmp
#define cmp(op, n) Ll(n##_u) {\
  ob n = getnum(Argc), *xs = Argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, N1); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)
#define OP2(nom, x) OP(nom, x, 2)
////
/// Load Instructions
//
// constants
OP1(unit, nil)
OP1(one, N1)
OP1(zero, N0)
// immediate value from thread
OP2(imm, (ob) ip[1].ll)

// indexed references
#define Ref(b) (*(ob*)((ob)(b)+(ob)ip[1].ll-Num))
// pointer arithmetic works because fixnums are premultiplied by W

// function arguments
OP2(arg, Ref(Argv))
OP1(arg0, Argv[0])
OP1(arg1, Argv[1])

// local variables
OP2(loc, Ref(((ob*)Locs)))
OP1(loc0, ((ob*)Locs)[0])
OP1(loc1, ((ob*)Locs)[1])

// closure variables
OP2(clo, Ref(((ob*)Clos)))
OP1(clo0, ((ob*)Clos)[0])
OP1(clo1, ((ob*)Clos)[1])

////
/// Store Instructions
//
// stack push
Vm(push) { Have1(); return *--sp = xp, ApN(1, xp); }

// set a local variable
Vm(loc_) { return
  Ref(((ob*)Locs)) = xp,
  ApN(2, xp); }

// set a global variable
Vm(tbind) { return
  CallC(tbl_set(v, v->glob[Topl], (ob) ip[1].ll, xp)),
  ApN(2, xp); }

// allocate local variable array
Vm(locals) {
  ob *t = hp;
  uintptr_t n = getnum((ob) ip[1].ll);
  Have(n + 3); return
    hp += n + 2,
    setw(t, nil, n),
    t[n] = 0,
    *--sp = t[n+1] = (ob) t,
    ApN(2, xp); }

// late binding
// long b/c it does the "static" type and arity checks
// that would have been done by the compiler if the function
// had been bound early.
Vm(lbind) {
  ob w = (ob) ip[1].ll, d = AB(w), y = A(w);
  if (!(w = tbl_get(v, d, xp = BB(w)))) {
    char *nom = nilp(getsym(xp)->nom) ? "()" : getstr(getsym(xp)->nom)->text;
    return Pack(), err(v, "free variable : %s", nom); }
  xp = w;
  if (y != putnum(sizeof(ob))) Tc(xp, getnum(y)); // do the type check
  // omit the arity check if possible
  if (ip[2].ll == call || ip[2].ll == rec) {
    yo x = gethom(xp);
    if (x[0].ll == arity && ip[3].ll >= x[1].ll)
      xp = (ob) (x + 2); }
  return
    ip[0].ll = imm,
    ip[1].ll = (ll*) xp,
    ApN(2, xp); }

// hash tables
Vm(tblg) {
  Arity(2);
  TypeCheck(Argv[0], Tbl);
  return xp = tbl_get(v, Argv[0], Argv[1]),
         ApC(ret, xp ? xp : nil); }

OP1(tget, (xp = tbl_get(v, xp, *sp++)) ? xp : nil)
OP1(thas, tbl_get(v, xp, *sp++) ? N1 : nil)
OP1(tlen, putnum(gettbl(xp)->len))

Vm(tkeys) { return
  CallC(v->xp = tblkeys(v, xp)),
  !xp ? 0 : ApN(1, xp); }

Vm(tblc) {
  Arity(2);
  TypeCheck(Argv[0], Tbl);
  return xp = tbl_get(v, Argv[0], Argv[1]),
         ApC(ret, xp ? N1 : nil); }

static ob tblss(em v, intptr_t i, intptr_t l) {
  fr fp = (fr) v->fp;
  return
    i > l-2 ? fp->argv[i-1]:
    !tbl_set(v, v->xp, fp->argv[i], fp->argv[i+1]) ? 0 :
    tblss(v, i+2, l); }

Vm(tbls) {
  Arity(1);
  xp = *Argv;
  TypeCheck(xp, Tbl);
  return
    CallC(v->xp = tblss(v, 1, getnum(Argc))),
    !xp ? 0 : ApC(ret, xp); }

Vm(tblmk) {
  return Pack(),
    (v->xp = table(v)) &&
    (xp = tblss(v, 0, getnum(Argc))) ?
      (Unpack(), ApC(ret, xp)) :
      0; }

Vm(tblks) {
  Arity(1);
  TypeCheck(*Argv, Tbl);
  return
    CallC(v->xp = tblkeys(v, *Argv)),
    !xp ? 0 : ApC(ret, xp); }

Vm(tbll) {
  Arity(1);
  TypeCheck(*Argv, Tbl);
  return ApC(ret, putnum(gettbl(*Argv)->len)); }

Vm(tset) {
  ob x = *sp++, y = *sp++;
  return
    CallC(v->xp = tbl_set(v, xp, x, y)),
    !xp ? 0 : ApN(1, xp); }

// pairs
OP1(car, A(xp)) OP1(cdr, B(xp))
Vm(cons) { Have1(); return
  hp[0] = xp,
  hp[1] = *sp++,
  xp = puttwo(hp),
  hp += 2,
  ApN(1, xp); }

Vm(car_u) {
  Arity(1);
  TypeCheck(*Argv, Two);
  return ApC(ret, A(*Argv)); }

Vm(cdr_u) {
  Arity(1);
  TypeCheck(*Argv, Two);
  return ApC(ret, B(*Argv)); }

Ll(cons_u) {
  Arity(2); Have(2); two w; return
    w = (two) hp,
    hp += 2,
    w->a = Argv[0], w->b = Argv[1],
    ApC(ret, puttwo(w)); }

// this is used to create closures.
Ll(take) {
  uintptr_t n = getnum((ob) ip[1].ll);
  Have(n + 2);
  ob *t = hp; return
    hp += n + 2,
    cpyw(t, sp, n),
    sp += n,
    t[n] = 0,
    t[n+1] = (ob) t,
    ApC(ret, (ob) t); }

static Ll(clos) { return
  Clos = (ob) ip[1].ll,
  ApY((ob) ip[2].ll, xp); }

// finalize function instance closure
static Ll(clos1) { return
  ip->ll = clos,
  ip[1].ll = (ll*) xp,
  ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
static Vm(clos0) {
  ob *ec  = (ob*) ip[1].ll,
     arg = ec[0],
     loc = ec[1];
  uintptr_t adic = nilp(arg) ? 0 : getnum(*(ob*)arg);
  Have(Width(fr) + adic + 1);
  intptr_t off = (ob*) fp - sp;
  ip->ll = clos1;
  sp -= adic;
  cpyw(sp, (ob*) arg + 1, adic);
  ec = (ob*) ip[1].ll;
  fp = (void*) ((fr) sp - 1);
  sp = (ob*) fp;
  Retp = (ob) ip;
  Subr = putnum(off);
  Argc = putnum(adic);
  Clos = ec[2];
  if (!nilp(loc)) *--sp = loc;
  return ApY(ec[3], xp); }

// the next few functions create and store
// lexical environments.
static Vm(encl) {
  intptr_t n = getnum(Argc);
  n += n ? 14 : 11;
  Have(n);
  ob x = (ob) ip[1].ll, arg = nil;
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

  return
    t[0].ll = (ll*) arg,
    t[1].ll = (ll*) xp, // Locs or nil
    t[2].ll = (ll*) Clos,
    t[3].ll = (ll*) B(x),
    t[4].ll = NULL,
    t[5].ll = (ll*) t,

    at[0].ll = clos0,
    at[1].ll = (ll*) t,
    at[2].ll = (ll*) A(x),
    at[3].ll = NULL,
    at[4].ll = (ll*) at,

    ApN(2, (ob) at); }

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
