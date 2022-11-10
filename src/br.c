#include "la.h"
#include "cmp.h"
#include "vm.h"
#include "two.h"
#include <string.h>

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
  size_t adic = 0;
  while (twop(xp)) xp = B(xp), adic++;
  Have(adic);
  ip = (mo) fp->argv[0];
  xp = fp->argv[1];
  sf subd = fp->subd;
  mo retp = fp->retp;
  sp = fp->argv + fp->argc - adic;
  for (size_t j = 0; j < adic; sp[j++] = A(xp), xp = B(xp));
  fp = (sf) sp - 1;
  sp = (ob*) fp;
  fp->retp = retp;
  fp->argc = adic;
  fp->subd = subd;
  fp->clos = (ob*) nil;
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
  for (size_t i = xp; i--; fp->argv[i] = sp[i]);
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
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  return ApY((test) ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(br1, nilp(xp), FF, xp, GF, xp)
Br(br0, nilp(xp), GF, xp, FF, xp)

Br(bre, eql(v, xp, *sp++), GF, T, FF, nil)
Br(brn, eql(v, xp, *sp++), FF, T, GF, nil)

Br(brl,   *sp++ <  xp, GF, xp, FF, nil)
Br(brl2,  *sp++ <  xp, FF, xp, GF, nil)
Br(brle,  *sp++ <= xp, GF, xp, FF, nil)
Br(brle2, *sp++ <= xp, FF, xp, GF, nil)
Br(brg,   *sp++ >  xp, GF, xp, FF, nil)
Br(brg2,  *sp++ >  xp, FF, xp, GF, nil)
Br(brge,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
