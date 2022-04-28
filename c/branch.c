#include "lips.h"
#include "terp.h"
#include "mem.h"
static terp recne;

////
/// Branch Instructions
//
// unconditional jump
Vm(jump) { Ap((obj) H(ip)[1], xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  if (test) Ap((obj)a(H(ip)),x);\
  else Ap((obj)b(H(ip)),y); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq,  eql(*sp++, xp), GF, ok, FF, nil)
Br(brne,  eql(*sp++, xp), FF, ok, GF, nil)

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
Vm(ret) {
 ip = Retp;
 sp = (mem) ((i64) Argv + Argc - Num);
 fp = (mem) ((i64)   sp + Subr - Num);
 Next(0); }

// "inner" function call
Vm(call) {
 Have(Width(frame));
 obj adic = (obj) H(ip)[1];
 i64 off = fp - (mem) ((i64) sp + adic - Num);
 fp = sp -= Width(frame);
 Retp = ip + 2 * word;
 Subr = _N(off);
 Clos = nil;
 Argc = adic;
 Ap(xp, nil); }

// tail call
Vm(rec) {
 if (Argc != (ip = (obj) H(ip)[1])) Jump(recne);
 cpy64(Argv, sp, ip = getnum(ip));
 sp = fp;
 Ap(xp, nil); }

// tail call with different arity
static Vm(recne) {
 v->xp = Subr, v->ip = Retp; // save return info
 fp = Argv + N(Argc - ip);
 cpy64r(fp, sp, N(ip)); // copy from high to low
 sp = fp -= Width(frame);
 Retp = v->ip;
 Argc = ip;
 Subr = v->xp;
 ip = xp;
 Clos = xp = nil;
 Next(0); }
