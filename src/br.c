#include "i.h"

Vm(yield_status) {
  enum status r = v->xp;
  return Pack(), r; }

Vm(xok) { return Pack(), Ok; }
Vm(xdom) { return Pack(), DomainError; }
////
/// Branch Instructions
//
//
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
Br(brl, *sp++ < xp, GF, FF)
Br(brg, *sp++ > xp, GF, FF)
Br(brle, *sp++ <= xp, GF, FF)
Br(brge, *sp++ >= xp, GF, FF)

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
  Have(Width(struct frame));
  frame subd = fp;
  return
    fp = (sf) sp - 1,
    sp = (ob*) fp,
    fp->argc = getnum(ip[1].ap),
    fp->retp = ip + 2,
    fp->subd = subd,
    fp->clos = (ob*) nil,
    ApY(xp, nil); }

// tail calls
Vm(rec) {
  size_t adic = getnum(GF(ip));
  // save return address
  sf subd = fp->subd;
  mo retp = fp->retp;
  return
    fp = (frame) (fp->argv + fp->argc - adic) - 1,
    cpyw_r2l(fp->argv, sp, adic),
    sp = (ob*) fp,
    fp->retp = retp,
    fp->subd = subd,
    fp->argc = adic,
    fp->clos = (ob*) nil,
    ApY((mo) xp, nil); }

Vm(ap_f) {
  if (fp->argc < 2) return Yield(ArityError, putnum(2));
  if (!homp(fp->argv[0])) return Yield(DomainError, xp);
  xp = fp->argv[1];
  size_t adic = llen(xp);
  Have(adic);
  ip = (mo) fp->argv[0];
  frame subd = fp->subd;
  mo retp = fp->retp;
  fp = (frame) (fp->argv + fp->argc - adic) - 1;
  sp = (ob*) fp;
  fp->retp = retp;
  fp->argc = adic;
  fp->subd = subd;
  fp->clos = (ob*) nil;
  for (ob *i = fp->argv; adic--; *i++ = A(xp), xp = B(xp));
  return ApY(ip, nil); }
