#include "lips.h"
#include "terp.h"
#include "hom.h"
#include "err.h"

#define div0_err_msg "%d / 0"

#define mm_u(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op N(x)){\
  x = *xs++; TC(x, Num);}\
 GO(ret, _N(xp));}

#define mm_u0(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op N(x)){\
  x = *xs++; TC(x, Num);\
  if (x == _N(0)) Jump(nope, div0_err_msg, xp);}\
 GO(ret, _N(xp));}

#define UBINOP(nom, dflt, op)\
 VM(nom##_u) { mm_u(N(ARGC), ARGV, dflt, op); }

// arithmetic
BINOP(add,  xp + *sp++ - Num)
UBINOP(add, 0, +)
BINOP(bor,  xp | *sp++)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  _N(N(*sp++)  * N(xp)))
UBINOP(mul, 1, *)
BINOP(band, xp & *sp++)
UBINOP(band, -1, &)

OP1(neg, _N(-N(xp)))
BINOP(sub,  *sp++ - xp + Num)
VM(sub_u) {
 if (!(xp = N(ARGC))) GO(ret, _N(0));
 TC(*ARGV, Num);
 if (xp == 1) GO(ret, _N(-N(*ARGV)));
 mm_u(xp-1,ARGV+1,N(*ARGV),-); }

BINOP(sar,  _N(N(*sp++) >> N(xp)))
VM(sar_u) {
 if (ARGC == _N(0)) GO(ret, _N(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), >>); }

BINOP(sal,  _N(N(*sp++) << N(xp)))
VM(sal_u) {
 if (ARGC == _N(0)) GO(ret, _N(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), <<); }

VM(dqv) {
 if (xp == _N(0)) Jump(nope, div0_err_msg, N(*sp));
 xp = _N(N(*sp++) / N(xp));
 NEXT(1); }
VM(div_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),/); }

VM(mod) {
 if (xp == _N(0))
   Jump(nope, div0_err_msg, N(*sp));
 xp = _N(N(*sp++) % N(xp));
 NEXT(1); }
VM(mod_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),%); }

VM(rnd_u) {
  GO(ret, _N(lcprng(&v->seed))); }
