#include "la.h"

////
/// Branch Instructions
//
// calling and returning
//
// return from a function
Vm(ret) { return
  ip = fp->retp,
  sp = fp->argv + fp->argc,
  fp = fp->subd,
  ApY(ip, xp); }

// normal function call
Vm(call) {
  Have(wsizeof(struct sf));
  sf subd = fp;
  sp = (ob*) (fp = (sf) sp - 1);
  fp->argc = getnum(GF(ip));
  fp->retp = FF(ip);
  fp->subd = subd;
  fp->clos = (ob*) nil;
  return ApY(xp, nil); }

// tail calls
//

Vm(rec) {
  size_t adic = getnum(GF(ip));
  // save return address
  sf subd = fp->subd;
  mo retp = fp->retp;
  // reset fp
  fp = (sf) (fp->argv + fp->argc - adic) - 1;
  // copy the args high to low BEFORE repopulating fp.
  cpyw_r2l(fp->argv, sp, adic);
  sp = (ob*) fp;
  // populate fp
  fp->retp = retp;
  fp->subd = subd;
  fp->argc = adic;
  fp->clos = (ob*) nil;
  return ApY((mo) xp, nil); }

Vm(ap_f) {
  ArityCheck(2);
  Check(homp(fp->argv[0]));
  xp = fp->argv[1];
  size_t adic = llen(xp);
  Have(adic);
  ip = (mo) fp->argv[0];
  sf subd = fp->subd;
  mo retp = fp->retp;
  sp = (ob*) (fp = (sf) (fp->argv + fp->argc - adic) - 1);
  fp->retp = retp;
  fp->argc = adic;
  fp->subd = subd;
  fp->clos = (ob*) nil;
  for (ob *i = fp->argv; adic--; xp = B(xp)) *i++ = A(xp);
  return ApY(ip, nil); }

// unconditional jump
Vm(jump) { return ApY(GF(ip), xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, b) Vm(nom) { return\
  ApY((test) ? (ob) a(ip) : (ob) b(ip), xp); }
// combined test/branch instructions
Br(br1, nilp(xp), FF, GF)
Br(br0, nilp(xp), GF, FF)
Br(bre, eql(v, xp, *sp++), GF, FF)
Br(brn, eql(v, xp, *sp++), FF, GF)
Br(brl,   *sp++ <  xp, GF, FF)
Br(brle,  *sp++ <= xp, GF, FF)
Br(brg,   *sp++ >  xp, GF, FF)
Br(brge,  *sp++ >= xp, GF, FF)
