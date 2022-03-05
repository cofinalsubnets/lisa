#include "lips.h"
#include "terp.h"
#include "mem.h"
#include "hom.h"
#include "cmp.h"
#include "vec.h"
static terp recne;

////
/// Branch Instructions
//
// unconditional jump
Vm(jump) { Ap((obj) H(ip)[1], xp); }

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(test, a, x, b, y) {\
  if (test) Ap((obj)a(H(ip)),x);\
  else Ap((obj)b(H(ip)),y); }

// combined test/branch instructions
Vm(branch)  Br(xp != nil, GF, xp, FF, xp)
Vm(barnch)  Br(xp == nil, GF, xp, FF, xp)

Vm(breq)    Br(eql(*sp++, xp), GF, ok, FF, nil)
Vm(brne)    Br(eql(*sp++, xp), FF, ok, GF, nil)

Vm(brlt)    Br(*sp++ < xp,  GF, xp,  FF, nil)
Vm(brlt2)   Br(*sp++ < xp,  FF, xp,  GF, nil)

Vm(brlteq)  Br(*sp++ <= xp, GF, xp,  FF, nil)
Vm(brlteq2) Br(*sp++ <= xp, FF, xp,  GF, nil)

Vm(brgt)    Br(*sp++ > xp, GF, xp, FF, nil)
Vm(brgt2)   Br(*sp++ > xp, FF, xp, GF, nil)

Vm(brgteq)  Br(*sp++ >= xp, GF, xp, FF, nil)
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
 Retp = ip + W2;
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

// call a continuation
Vm(cont) {
 vec t = V((obj) H(ip)[1]);
 Have(t->len - 1);
 xp = N(Argc) == 0 ? nil : *Argv;
 i64 off = N(t->xs[0]);
 sp = v->pool + v->len - (t->len - 1);
 fp = sp + off;
 cpy64(sp, t->xs+1, t->len-1);
 Jump(ret); }
