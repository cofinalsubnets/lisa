#include "la.h"

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

// calling and returning
Ll(ap_u) {
  ArityCheck(2);
  ip = (mo) fp->argv[0];
  TypeCheck((ob)ip, Hom);
  ob x = fp->argv[1];
  uintptr_t adic = llen(x);
  if (Slack < adic) return Pray(adic);
  ob off = fp->subd, rp = fp->retp;
  sp = fp->argv + getnum(fp->argc) - adic;
  for (N j = 0; j < adic; sp[j++] = A(x), x = B(x));
  return
    fp = (fr) sp - 1,
    sp = (ob*) fp,
    fp->retp = rp,
    fp->argc = putnum(adic),
    fp->subd = off,
    fp->clos = nil,
    ApY(ip, nil); }

Ll(vararg) {
  Z reqd = getnum((ob) ip[1].ll),
    vdic = getnum(fp->argc) - reqd;
  ArityCheck(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    if (Slack == 0) return Pray(1);
    cpyw((ob*)fp - 1, fp, Width(fr) + getZ(fp->argc)),
    fp = (fr) ((ob*) fp - 1),
    sp = (ob*) fp,
    fp->argc += sizeof(ob),
    fp->argv[reqd] = nil;
    return ApN(2, xp); }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  if (Slack < 2 * vdic) return Pray(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (N i = vdic; i--;)
    t[i].a = fp->argv[reqd + i],
    t[i].b = put2(t+i+1);
  return
    t[vdic-1].b = nil,
    fp->argv[reqd] = put2(t),
    ApN(2, xp); }

static ll recne;

// return from a function
Ll(ret) { return
  ip = (mo) fp->retp,
  sp = (ob*) ((ob) fp->argv + fp->argc - Num),
  fp = (fr) ((ob) sp + fp->subd - Num),
  ApY(ip, xp); }

// "inner" function call
Ll(call) {
  if (Slack < Width(fr)) return Pray(Width(fr));
  Z adic = getnum((ob) ip[1].ll),
    off = (ob*) fp - (sp + adic);
  return
    fp = (fr) sp - 1,
    sp = (ob*) fp,
    fp->retp = (ob) (ip + 2),
    fp->subd = putnum(off),
    fp->clos = nil,
    fp->argc = putnum(adic),
    ApY(xp, nil); }

// tail call
Dt(rec) {
  ip = (dt) ip[1].ll;
  if (fp->argc != (ob) ip) return ApC(recne, xp);
  cpyw(fp->argv, sp, getnum((ob) ip));
  sp = (ob*) fp;
  return ApY(xp, nil); }

// tail call with different arity
static Dt(recne) {
  v->xp = fp->subd;
  v->ip = (dt) fp->retp; // save return info
  fp = (fr) (fp->argv + getZ(fp->argc - (ob) ip));
  rcpyw(fp, sp, getZ((ob) ip)); // copy from high to low
  sp = (ob*) (((fr) fp) - 1);
  fp = (fr) sp;
  fp->retp = (ob) v->ip;
  fp->argc = (ob) ip;
  fp->subd = v->xp;
  fp->clos = nil;
  return ApY(xp, nil); }

////
/// Branch Instructions
//
// unconditional jump
Dt(jump) { return ApY((ob) ip[1].ll, xp); }

////
/// Load Instructions
//

// constants
Op(1, one, putZ(1))
Op(1, zero, putZ(0))
// immediate value from thread
Op(2, imm, (ob) ip[1].ll)

// indexed references
#define Ref(b) (*(ob*)((ob)(b)+(ob)ip[1].ll-Num))
// pointer arithmetic works because fixnums are premultiplied by W

// function arguments
Op(2, arg, Ref(fp->argv))
Op(1, arg0, fp->argv[0])
Op(1, arg1, fp->argv[1])

// local variables
Op(2, loc, Ref(Locs))
Op(1, loc0, Locs[0])
Op(1, loc1, Locs[1])

// closure variables
Op(2, clo, Ref(Clos))
Op(1, clo0, Clos[0])
Op(1, clo1, Clos[1])

////
/// Store Instructions
//
// stack push
Vm(push) {
  return Slack == 0 ? Pray(1) :
    (*--sp = xp, ApN(1, xp)); }

// dup top of stack
Vm(dupl) {
  return Slack == 0 ? Pray(1) :
    (--sp, sp[0] = sp[1], ApN(1, xp)); }


// set a local variable
Vm(loc_) {
  Ref(Locs) = xp;
  return ApN(2, xp); }

// set a module variable
Vm(tbind) {
  ob a = (ob) ip[1].ll;
  Pack();
  v->xp = tbl_set(v, A(a), B(a), xp);
  Unpack();
  return !xp ? 0 : ApN(2, xp); }

// allocate local variable array
Vm(locals) {
  ob *t = hp;
  uintptr_t n = getZ((ob) ip[1].ll);
  if (Slack < n + 3) return Pray(n + 3);
  hp += n + 2,
  setw(t, nil, n),
  t[n] = 0,
  *--sp = t[n+1] = (ob) t;
  return ApN(2, xp); }

// late binding
// long b/c it does the "static" type and arity checks
// that would have been done by the compiler if the function
// had been bound early.
Vm(rslv) {
  ob w = (ob) ip[1].ll, d = AB(w), typ = A(w) >> TagBits;
  if (!(w = tbl_get(v, d, xp = BB(w)))) {
    Pack();
    return err(v, xp, "referenced free variable"); }
  xp = w;
  if (typ != sizeof(ob)) TypeCheck(xp, typ);
  // omit the arity check if possible
  if ((ip[2].ll == call || ip[2].ll == rec) && // xp will be a hom
      ptr(xp)[0] == (ob) arity &&
      ptr(ip)[3] >= ptr(xp)[1])
    xp = (ob) (ptr(xp) + 2);
  return ip[0].ll = imm,
         ip[1].ll = (ll*) xp,
         ApN(2, xp); }
// this is used to create closures.
Dt(take) {
  uintptr_t n = getnum((ob) ip[1].ll);
  if (Slack < n + 2) return Pray(n + 2);
  ob *t = hp;
  hp += n + 2,
  cpyw(t, sp, n),
  sp += n,
  t[n] = 0,
  t[n+1] = (ob) t;
  return ApC(ret, (ob) t); }

static Dt(clos) {
  fp->clos = (ob) ip[1].ll;
  return ApY((ob) ip[2].ll, xp); }

// finalize function instance closure
static Dt(clos1) {
  ip->ll = clos;
  ip[1].ll = (host*) xp;
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
  ob *ec  = (ob*) ip[1].ll,
     arg = ec[0],
     loc = ec[1];
  N adic = nilp(arg) ? 0 : getnum(*(ob*)arg);
  if (Slack < Width(fr) + adic + 1)
    return Pray(Width(fr) + adic + 1);
  Z off = (ob*) fp - sp;
  ip->ll = clos1;
  sp -= adic;
  cpyw(sp, (ob*) arg + 1, adic);
  ec = (ob*) ip[1].ll;
  sp = (ob*) (fp = (fr) sp - 1);
  fp->retp = (ob) ip;
  fp->subd = putnum(off);
  fp->argc = putnum(adic);
  fp->clos = ec[2];
  if (!nilp(loc)) *--sp = loc;
  return ApY(ec[3], xp); }

// the next few functions create and store
// lexical environments.
// FIXME magic numbers
static Vm(encl) {
  intptr_t m = getnum(fp->argc),
           n = m + (m ? 14 : 11);
  if (Slack < n) return Pray(n);
  ob x = (ob) ip[1].ll, arg = nil, *block = hp;
  hp += n;
  if (m) {
    n -= 11;
    ob *t = block;
    block += n;
    for (t[n-2] = 0,
         t[n-1] = (ob) t,
         t[0] = putnum(n-=3);
         n--;
         t[n+1] = fp->argv[n]);
    arg = (ob) t; }

  ob *t = (ob*) block, // compiler thread closure array (1 length 5 elements)
     *at = t + 6; // compiler thread (1 instruction 2 data 2 tag)

  t[0] =  arg,
  t[1] =  xp, // Locs or nil
  t[2] =  fp->clos,
  t[3] =  B(x),
  t[4] = 0,
  t[5] = (ob) t,

  at[0] = (ob) clos0,
  at[1] = (ob) t,
  at[2] =  A(x),
  at[3] = 0,
  at[4] = (ob) at;

  return ApN(2, (ob) at); }

Ll(encll) { return ApC(encl, (ob) Locs); }
Ll(encln) { return ApC(encl, nil); }

ob hnom(pt v, ob x) {
  host *k = gethom(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return hnom(v, (ob) gethom(x)[2].ll);
  ob* h = (ob*) gethom(x);
  while (*h) h++;
  x = h[-1];
  int inb = (ob*) x >= v->pool && (ob*) x < v->pool+v->len;
  return inb ? x : nil; }

Ll(cwm_u) { return ApC(ret, v->wns); }
Ll(popd_u) {
  xp = B(v->wns);
  if (twop(xp)) v->wns = xp;
  return ApC(ret, nil); }

ob refer(pt v, ob _) {
  ob x, mod = v->wns;
  for (; twop(mod); mod = B(mod))
    if ((x = tbl_get(v, A(mod), _))) return x;
  return 0; }

#include <string.h>

bool eql(ob a, ob b) {
  return a == b || (Q(a) == Q(b) &&
    ((twop(a) && eql(A(a), A(b)) && eql(B(a), B(b))) ||
     (strp(a) && getstr(a)->len == getstr(b)->len &&
      0 == strcmp(getstr(a)->text, getstr(b)->text)))); }

#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)

BINOP(eq, eql(xp, *sp++) ? T : nil)
#define cmp(n, op) BINOP(n, op(*sp++, xp) ? xp : nil)
cmp(lt, LT) cmp(lteq, LE) cmp(gteq, GE) cmp(gt, GT)
#undef cmp
#define cmp(op, n) Ll(n##_u) {\
  ob n = getnum(fp->argc), *xs = fp->argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, T); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)

// type predicates
#define Tp(t)\
  Ll(t##pp) { return ApN(1, (t##p(xp)?T:nil)); }\
  Ll(t##p_u) {\
    for (ob *xs = fp->argv, *l = xs + getnum(fp->argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Ll(nom) {\
  return ApY(test ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq, eql(*sp++, xp), GF, T, FF, nil)
Br(brne, eql(*sp++, xp), FF, T, GF, nil)

Br(brlt,    *sp++ < xp,  GF, xp, FF, nil)
Br(brlt2,   *sp++ < xp,  FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ > xp,  GF, xp, FF, nil)
Br(brgt2,   *sp++ > xp,  FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt

#include <stdarg.h>

Vm(oom_err) { return Pack(),
  err(v, 0, "oom with %d words", v->len); }

Ll(dom_err) { return Pack(),
  err(v, 0, "is undefined"); }

Ll(ary_err) { return Pack(),
  err(v, 0, "takes %d arguments", getZ(xp)); }

// type/arity checking
#define DTc(n, t) Vm(n) {\
  return IsA(t, xp) ? ApN(1, xp) : ApC(ary_err, xp); }
DTc(idZ, Num)
DTc(idH, Hom)
DTc(idT, Tbl)
DTc(id2, Two)
Ll(arity) {
  ob reqd = (ob) ip[1].ll;
  return reqd > fp->argc ?
    ApC(ary_err, reqd) :
    ApN(2, xp); }

static u0 show_call(ps v, mo ip, co fp) {
  fputc('(', stderr);
  tx(v, stderr, (ob) ip);
  for (uintptr_t i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr),
    tx(v, stderr, fp->argv[i++]);
  fputc(')', stderr); }

static Inline u1 atop(pt v, co fp) {
  return ptr(fp) == v->pool + v->len; }

static ob restart(pt v) {
  v->fp = (fr) (v->pool + v->len);
  v->sp = (ob*) v->fp;
  v->xp = nil;
  v->ip = (dt) nil;
  return 0; }

NoInline ob err(pt v, ob x, const char *msg, ...) {
  if (x || msg) {
    dt ip = v->ip;
    fr fp = v->fp;
    // error line
    fputs("# ", stderr);
    if (!atop(v, fp)) show_call(v, ip, fp), fputs(" ", stderr);
    if (msg) {
      va_list xs;
      va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
      if (x) fputs(" ", stderr); }
    if (x) tx(v, stderr, x);
    fputc('\n', stderr);

    // backtrace
    if (!atop(v, fp)) for (;;) {
      ip = (dt) fp->retp, fp = (fr)
        ((ob*) (fp + 1) + getnum(fp->argc)
                        + getnum(fp->subd));
      if (atop(v, fp)) break; else
        fputs("# in ", stderr),
        show_call(v, ip, fp),
        fputc('\n', stderr); } }

  // reset and yield
  return restart(v); }
