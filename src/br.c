#include "la.h"

////
/// Branch Instructions
//
// calling and returning
Vm(call) {
  Have(wsizeof(struct sf));
  sf subd = fp;
  fp = (sf) sp - 1;
  sp = (ob*) fp;
  fp->argc = getnum(GF(ip));
  fp->retp = FF(ip);
  fp->subd = subd;
  fp->clos = (ob*) nil;
  return ApY(xp, nil); }

Vm(ap_f) {
  ArityCheck(2);
  Check(homp(fp->argv[0]));
  xp = fp->argv[1];
  size_t adic = llen(xp);
  Have(adic);
  ip = (mo) fp->argv[0];
  sf subd = fp->subd;
  mo retp = fp->retp;
  fp = (sf) (fp->argv + fp->argc - adic) - 1;
  sp = (ob*) fp;
  fp->retp = retp;
  fp->argc = adic;
  fp->subd = subd;
  fp->clos = (ob*) nil;
  for (size_t j = 0; j < adic; xp = B(xp))
    fp->argv[j++] = A(xp);
  return ApY(ip, nil); }

// return from a function
Vm(ret) { return
  ip = fp->retp,
  sp = fp->argv + fp->argc,
  fp = fp->subd,
  ApY(ip, xp); }

// tail calls
//

// if the adicity is different we need to do a little more.
static NoInline Vm(recn) {
  // save return address
  sf subd = fp->subd;
  mo retp = fp->retp;
  // reset fp
  fp = (sf) (fp->argv + fp->argc - xp) - 1;
  // copy the args high to low BEFORE populating fp.
  cpyw_r2l(fp->argv, sp, xp);
  sp = (ob*) fp;
  // populate fp
  fp->retp = retp;
  fp->subd = subd;
  fp->argc = xp;
  fp->clos = (ob*) nil;
  return ApY(ip, nil); }

Vm(rec) {
  ob _ = getnum(GF(ip));
  ip = (mo) xp;
  xp = _;
  if (fp->argc != xp) return ApC(recn, xp);
  // fast case where the calls have the same number of args
  while (xp--) fp->argv[xp] = sp[xp];
  sp = (ob*) fp;
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
Br(brl2,  *sp++ <  xp, FF, GF)
Br(brle,  *sp++ <= xp, GF, FF)
Br(brle2, *sp++ <= xp, FF, GF)
Br(brg,   *sp++ >  xp, GF, FF)
Br(brg2,  *sp++ >  xp, FF, GF)
Br(brge,  *sp++ >= xp, GF, FF)
// brgteq2 is brlt
