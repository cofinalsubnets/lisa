#include "lisa.h"
#include "vm.h"

////
/// Branch Instructions
//
// calling and returning
Vm(call) {
  Have(Width(fr));
  size_t adic = (ob) GF(ip),
         subd = (ob*) fp - (sp + getnum(adic));
  fp = (fr) sp - 1;
  sp = (ob*) fp;
  fp->retp = (ob) FF(ip);
  fp->subd = putnum(subd);
  fp->clos = nil;
  fp->argc = adic;
  return ApY(xp, nil); }

Vm(ap_u) {
  ArityCheck(2);
  Check(homp(fp->argv[0]));
  size_t adic = llen(fp->argv[1]);
  Have(adic);
  ip = (mo) fp->argv[0];
  xp = fp->argv[1];
  ob subd = fp->subd, retp = fp->retp;
  sp = fp->argv + getnum(fp->argc) - adic;
  for (size_t j = 0; j < adic; sp[j++] = A(xp), xp = B(xp));
  fp = (fr) sp - 1;
  sp = (ob*) fp;
  fp->retp = retp;
  fp->argc = putnum(adic);
  fp->subd = subd;
  fp->clos = nil;
  return ApY(ip, nil); }

// return from a function
Vm(ret) { return
  ip = (mo) fp->retp,
  sp = fp->argv + getnum(fp->argc),
  fp = (fr) (sp + getnum(fp->subd)),
  ApN(0, xp); }

// tail calls
//
// fast case where the calls have the same number of args
static NoInline Vm(recq) {
  for (size_t i = getnum((ob) ip); i--; fp->argv[i] = sp[i]);
  return sp = (ob*) fp, ApY(xp, nil); }

Vm(rec) {
  ip = (mo) GF(ip);
  if (fp->argc == (ob) ip) return ApC(recq, xp);
  // save return address
  v->xp = fp->subd;
  v->ip = (mo) fp->retp;
  // set fp to the new argv address
  fp = (fr) (fp->argv + getnum(fp->argc - (ob) ip));
  // copy the args high to low
  rcpyw(fp, sp, getnum((ob) ip));
  // bump fp & set sp
  sp = (ob*) (--fp);
  // set return address & call info
  fp->retp = (ob) v->ip;
  fp->subd = v->xp;
  fp->argc = (ob) ip;
  fp->clos = nil;
  return ApY(xp, nil); }

// unconditional jump
Vm(jump) { return ApY(GF(ip), xp); }


// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  return ApY((test) ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq, eql(v, xp, *sp++), GF, T, FF, nil)
Br(brne, eql(v, xp, *sp++), FF, T, GF, nil)

Br(brlt,    *sp++ <  xp, GF, xp, FF, nil)
Br(brlt2,   *sp++ <  xp, FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ >  xp, GF, xp, FF, nil)
Br(brgt2,   *sp++ >  xp, FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
