#include "lisa.h"
#include "vm.h"

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
Vm(ap_u) {
  ArityCheck(2);
  TypeCheck(fp->argv[0], Hom);
  ip = (mo) fp->argv[0];
  ob x = fp->argv[1];
  size_t adic = llen(x);
  Have(adic);
  ob off = fp->subd, rp = fp->retp;
  sp = fp->argv + getnum(fp->argc) - adic;
  for (size_t j = 0; j < adic; sp[j++] = A(x), x = B(x));
  return
    fp = (fr) sp - 1,
    sp = (ob*) fp,
    fp->retp = rp,
    fp->argc = putnum(adic),
    fp->subd = off,
    fp->clos = nil,
    ApY(ip, nil); }

Vm(vararg) {
  intptr_t reqd = getnum((ob) ip[1].ll),
           vdic = getnum(fp->argc) - reqd;
  ArityCheck(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1();
    return
      cpyw((ob*)fp - 1, fp, Width(fr) + getnum(fp->argc)),
      fp = (fr) ((ob*) fp - 1),
      sp = (ob*) fp,
      fp->argc += sizeof(ob),
      fp->argv[reqd] = nil,
      ApN(2, xp); }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (size_t i = vdic; i--;
    t[i].a = fp->argv[reqd + i],
    t[i].b = puttwo(t+i+1));
  return
    t[vdic-1].b = nil,
    fp->argv[reqd] = puttwo(t),
    ApN(2, xp); }


// return from a function
Vm(ret) { return
  ip = (mo) fp->retp,
  sp = (ob*) ((ob) fp->argv + fp->argc - Num),
  fp = (fr) ((ob) sp + fp->subd - Num),
  ApY(ip, xp); }

// "inner" function call
Vm(call) {
  intptr_t adic = getnum((ob) ip[1].ll),
           off = (ob*) fp - (sp + adic);
  Have(Width(fr));
  return
    fp = (fr) sp - 1,
    sp = (ob*) fp,
    fp->retp = (ob) (ip + 2),
    fp->subd = putnum(off),
    fp->clos = nil,
    fp->argc = putnum(adic),
    ApY(xp, nil); }

// tail call
Vm(rec) {
  ip = (mo) ip[1].ll;
  if (fp->argc == (ob) ip) return
    cpyw(fp->argv, sp, getnum((ob) ip)),
    sp = (ob*) fp,
    ApY(xp, nil);
  return
    v->xp = fp->subd,
    v->ip = (mo) fp->retp, // save return info
    fp = (fr) (fp->argv + getnum(fp->argc - (ob) ip)),
    rcpyw(fp, sp, getnum((ob) ip)), // copy from high to low
    sp = (ob*) (--fp),
    fp->retp = (ob) v->ip,
    fp->argc = (ob) ip,
    fp->subd = v->xp,
    fp->clos = nil,
    ApY(xp, nil); }

Vm(disp) { return ApY(((ext)ip)->dtbl->ap, xp); }

////
/// Load Instructions
//

// constants
Vm(one) { return ApN(1, putnum(1)); }
Vm(zero) { return ApN(1, putnum(0)); }
// immediate value from thread
Vm(imm) { return xp = (ob) ip[1].ll, ApN(2, xp); }

// indexed references
#define Ref(b) (*(ob*)((ob)(b)+(ob)ip[1].ll-Num))
// pointer arithmetic works because fixnums are premultiplied by W

// function arguments
Vm(arg) { return xp = Ref(fp->argv), ApN(2, xp); }
Vm(arg0) { return ApN(1, fp->argv[0]); }
Vm(arg1) { return ApN(1, fp->argv[1]); }

// local variables
Vm(loc) { return xp = Ref(Locs), ApN(2, xp); }
Vm(loc0) { return ApN(1, Locs[0]); }
Vm(loc1) { return ApN(1, Locs[1]); }

// closure variables
Vm(clo) { return xp = Ref(Clos), ApN(2, xp); }
Vm(clo0) { return ApN(1, Clos[0]); }
Vm(clo1) { return ApN(1, Clos[1]); }

////
/// Store Instructions
// // stack push
Vm(push) {
  Have1();
  return *--sp = xp, ApN(1, xp); }

// dup top of stack
Vm(dupl) {
  Have1();
  return --sp, sp[0] = sp[1], ApN(1, xp); }

// set a local variable
Vm(loc_) { return Ref(Locs) = xp, ApN(2, xp); }

// set a module variable
Vm(tbind) { ob a; return
  a = (ob) ip[1].ll,
  Pack(),
  v->xp = tbl_set(v, A(a), B(a), xp),
  Unpack(),
  xp ? ApN(2, xp) : 0; }

// allocate local variable array
Vm(locals) {
  ob *t = hp, n = getnum((ob) ip[1].ll);
  Have(3);
  return
    hp += n + 2,
    setw(t, nil, n),
    t[n] = 0,
    *--sp = t[n+1] = (ob) t,
    ApN(2, xp); }

// late binding
// long b/c it does the "static" type and arity checks
// that would have been done by the compiler if the function
// had been bound early.
Vm(latebind) {
  ob w = (ob) ip[1].ll, d = AB(w), typ = A(w) >> TagBits;
  xp = BB(w);
  w = tbl_get(v, d, xp);
  if (!w) return ApC(nom_err, xp);
  if (typ != sizeof(ob)) TypeCheck(w, typ);
  xp = w;
  // omit the arity check if possible
  if ((ip[2].ll == call || ip[2].ll == rec) && // xp will be a hom
      ptr(xp)[0] == (ob) arity &&
      ptr(ip)[3] >= ptr(xp)[1])
    xp = (ob) (ptr(xp) + 2);
  return
    ip[0].ll = imm,
    ip[1].ll = (vm*) xp,
    ApN(2, xp); }

// this is used to create closures.
Vm(take) {
  ob *t, n = getnum((ob) ip[1].ll);
  Have(n + 2);
  return
    t = hp,
    hp += n + 2,
    cpyw(t, sp, n),
    sp += n,
    t[n] = 0,
    t[n+1] = (ob) t,
    ApC(ret, (ob) t); }

Vm(clos) { return
  fp->clos = (ob) ip[1].ll,
  ApY((ob) ip[2].ll, xp); }

// finalize function instance closure
Vm(clos1) { return
  ip->ll = clos,
  ip[1].ll = (vm*) xp,
  ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
Vm(clos0) {
  ob *ec = (ob*) ip[1].ll,
     arg = ec[0],
     loc = ec[1];
  intptr_t adic = nilp(arg) ? 0 : getnum(*(ob*)arg),
           req = Width(fr) + adic + 1;
  Have(req);

  intptr_t off = (ob*) fp - sp;
  ip->ll = clos1;
  sp -= adic;
  cpyw(sp, (ob*) arg + 1, adic);
  ec = (ob*) ip[1].ll;
  fp = (fr) sp - 1;
  sp = (ob*) fp;
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
  Have(n);
  ob x = (ob) ip[1].ll,
     arg = nil,
     *block = hp;
  hp += n;
  if (m) {
    n -= 11;
    arg = (ob) block;
    block += n;
    for (ptr(arg)[n-2] = 0,
         ptr(arg)[n-1] = (ob) arg,
         n -= 3,
         ptr(arg)[0] = putnum(n);
         n--;
         ptr(arg)[n+1] = fp->argv[n]); }

  ob *t = (ob*) block, // compiler thread closure array
     *at = t + 6; // compiler thread

  return
    t[0] = arg,
    t[1] = xp, // Locs or nil
    t[2] = fp->clos,
    t[3] = B(x),
    t[4] = 0,
    t[5] = (ob) t,

    at[0] = (ob) clos0,
    at[1] = (ob) t,
    at[2] = A(x),
    at[3] = 0,
    at[4] = (ob) at,

    ApN(2, (ob) at); }

Vm(encll) { return ApC(encl, (ob) Locs); }
Vm(encln) { return ApC(encl, nil); }

////
/// Branch Instructions
//
// unconditional jump
Vm(jump) { return ApY((ob) ip[1].ll, xp); }

#include <string.h>
// isolate the more complicated logic from the simple
// pointer comparisons so eql() doesn't touch the stack
// unless it has to
static NoInline bool eql2(ob a, ob b) {
  return 
    ((twop(a) && eql(A(a), A(b)) && eql(B(a), B(b))) ||
     (strp(a) && getstr(a)->len == getstr(b)->len &&
      0 == scmp(getstr(a)->text, getstr(b)->text))); }
bool eql(ob a, ob b) {
  return a == b ? 1 : TypeOf(a) != TypeOf(b) ? 0 : eql2(a, b); }

Vm(lt) { return xp = *sp++ < xp ? xp : nil, ApN(1, xp); }
Vm(lteq) { return xp = *sp++ <= xp ? xp : nil, ApN(1, xp); }
Vm(eq) { return xp = eql(xp, *sp++) ? T : nil, ApN(1, xp); }
Vm(gteq) { return xp = *sp++ >= xp ? xp : nil, ApN(1, xp); }
Vm(gt) { return xp = *sp++ > xp ? xp : nil, ApN(1, xp); }

// FIXME remove macros
#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
#define cmp(op, n) Vm(n##_u) {\
  ob n = getnum(fp->argc), *xs = fp->argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, T); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)

// type predicates
#define Tp(t)\
  Vm(t##pp) { return ApN(1, (t##p(xp)?T:nil)); }\
  Vm(t##p_u) {\
    for (ob *xs = fp->argv, *l = xs + getnum(fp->argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  return ApY(test ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq, eql(*sp++, xp), GF, T, FF, nil)
Br(brne, eql(*sp++, xp), FF, T, GF, nil)

Br(brlt,    *sp++ <  xp, GF, xp, FF, nil)
Br(brlt2,   *sp++ <  xp, FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ >  xp, GF, xp, FF, nil)
Br(brgt2,   *sp++ >  xp, FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
//
// type/arity checking
Vm(idZ) { return nump(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(idH) { return homp(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(idT) { return tblp(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(id2) { return twop(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(arity) {
  ob reqd = (ob) ip[1].ll;
  return fp->argc >= reqd ? ApN(2, xp) : ApC(ary_err, reqd); }
Vm(cwm_u) { return ApC(ret, v->topl); }

// errors
Vm(dom_err) { return Pack(), nope(v, "is undefined"); }
Vm(oom_err) { return Pack(), nope(v, "oom with %d words", v->len); }
Vm(ary_err) { return Pack(), nope(v, "takes %d parameters", getnum(xp)); }
Vm(nom_err) {
  xp = getsym(xp)->nom;
  return Pack(),
    nope(v, "referenced free variable `%s'", nilp(xp) ? 0 : getstr(xp)->text); }
