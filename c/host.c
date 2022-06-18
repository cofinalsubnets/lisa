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
  Arity(2);
  ip = (mo) fp->argv[0];
  TypeCheck((ob)ip, Hom);
  ob x = fp->argv[1];
  N adic = llen(x);
  Have(adic);
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
  Arity(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1(); return
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
  for (N i = vdic; i--;)
    t[i].a = fp->argv[reqd + i],
    t[i].b = puttwo(t+i+1);
  return
    t[vdic-1].b = nil,
    fp->argv[reqd] = puttwo(t),
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
  Have(Width(fr));
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
Ll(rec) { return
  ip = (mo) ip[1].ll,
  fp->argc != (ob) ip ? ApC(recne, xp) :
  (cpyw(fp->argv, sp, getnum((ob) ip)),
   sp = (ob*) fp,
   ApY(xp, nil)); }

// tail call with different arity
static Ll(recne) { return
  v->xp = fp->subd,
  v->ip = (mo) fp->retp, // save return info
  fp = (fr) (fp->argv + getnum(fp->argc - (ob) ip)),
  rcpyw(fp, sp, getnum((ob) ip)), // copy from high to low
  sp = (ob*) (((fr) fp) - 1),
  fp = (fr) sp,
  fp->retp = (ob) v->ip,
  fp->argc = (ob) ip,
  fp->subd = v->xp,
  ApY(xp, fp->clos = nil); }

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
#undef Br

////
/// Load Instructions
//

// constants
Op(1, one, putnum(1))
Op(1, zero, putnum(0))
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
Op(2, loc, Ref(((ob*)Locs)))
Op(1, loc0, ((ob*)Locs)[0])
Op(1, loc1, ((ob*)Locs)[1])

// closure variables
Op(2, clo, Ref(((ob*)fp->clos)))
Op(1, clo0, ((ob*)fp->clos)[0])
Op(1, clo1, ((ob*)fp->clos)[1])

////
/// Store Instructions
//
// stack push
Vm(push) { Have1(); return
  *--sp = xp,
  ApN(1, xp); }
// dup top of stack
Vm(dupl) { Have1(); return
  --sp,
  sp[0] = sp[1],
  ApN(1, xp); }


// set a local variable
Vm(loc_) { return
  Ref(((ob*)Locs)) = xp,
  ApN(2, xp); }

// set a module variable
Vm(tbind) {
  ob a = (ob) ip[1].ll;
  Pack();
  xp = tbl_set(v, A(a), B(a), xp);
  return !xp ? 0 : (Unpack(), ApN(2, xp)); }

// allocate local variable array
Vm(locals) {
  ob *t = hp;
  N n = getnum((ob) ip[1].ll);
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
Vm(rslv) {
  ob w = (ob) ip[1].ll, d = AB(w), typ = A(w) >> TagBits;
  if (!(w = tbl_get(v, d, xp = BB(w))))
    return Pack(), err(v, xp, "referenced free variable");
  xp = w;
  if (typ != sizeof(ob)) TypeCheck(xp, typ);
  // omit the arity check if possible
  if ((ip[2].ll == call || ip[2].ll == rec) && // xp will be a hom
      R(xp)[0] == (ob) arity &&
      R(ip)[3] >= R(xp)[1])
    xp = (ob) (R(xp) + 2);
  return ip[0].ll = imm,
         ip[1].ll = (ll*) xp,
         ApN(2, xp); }
// this is used to create closures.
Ll(take) {
  N n = getnum((ob) ip[1].ll);
  Have(n + 2);
  ob *t = hp; return
    hp += n + 2,
    cpyw(t, sp, n),
    sp += n,
    t[n] = 0,
    t[n+1] = (ob) t,
    ApC(ret, (ob) t); }

static Ll(clos) { return
  fp->clos = (ob) ip[1].ll,
  ApY((ob) ip[2].ll, xp); }

// finalize function instance closure
static Ll(clos1) { return
  ip->ll = clos,
  ip[1].ll = (host*) xp,
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
  N adic = nilp(arg) ? 0 : getnum(*(ob*)arg);
  Have(Width(fr) + adic + 1);
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
  Z m = getnum(fp->argc),
    n = m + (m ? 14 : 11);
  Have(n);
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

  return
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
    at[4] = (ob) at,

    ApN(2, (ob) at); }

Ll(encll) { return ApC(encl, Locs); }
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
