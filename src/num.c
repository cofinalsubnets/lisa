#include "lips.h"
#include "terp.h"
#include "hom.h"
#include "err.h"

// arithmetic
BINOP(add,  xp + *sp++ - Num)
BINOP(sub,  *sp++ - xp + Num)
BINOP(mul,  N_(N(*sp++)  * N(xp)))
BINOP(sar,  N_(N(*sp++) >> N(xp)))
BINOP(sal,  N_(N(*sp++) << N(xp)))
BINOP(band, xp & *sp++)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
OP1(neg, N_(-N(xp)))
VM(dqv) {
 if (xp == Pn(0)) Jump(nope, div0_err_msg, Gn(*sp));
 xp = Pn(Gn(*sp++) / Gn(xp));
 NEXT(1); }
VM(mod) {
 if (xp == Pn(0)) Jump(nope, div0_err_msg, Gn(*sp));
 xp = Pn(Gn(*sp++) % Gn(xp));
 NEXT(1); }


#define mm_u(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op Gn(x)){\
  x = *xs++; TC(x, Num);}\
 GO(ret, Pn(xp));}
#define mm_u0(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op Gn(x)){\
  x = *xs++; TC(x, Num);\
  if (x == Pn(0)) Jump(nope, div0_err_msg, xp);}\
 GO(ret, Pn(xp));}

#define UBINOP(nom, dflt, op)\
 VM(nom##_u) { mm_u(Gn(ARGC), ARGV, dflt, op); }
UBINOP(add, 0, +) UBINOP(mul, 1, *)
UBINOP(band, -1, &) UBINOP(bor, 0, |) UBINOP(bxor, 0, ^)

VM(sub_u) {
 if (!(xp = Gn(ARGC))) GO(ret, Pn(0));
 TC(*ARGV, Num);
 if (xp == 1) GO(ret, Pn(-Gn(*ARGV)));
 mm_u(xp-1,ARGV+1,Gn(*ARGV),-); }

VM(div_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),/); }
VM(mod_u) {
 if (!(xp = N(ARGC))) GO(ret, ok);
 TC(*ARGV, Num);
 mm_u0(xp-1,ARGV+1,N(*ARGV),%); }

VM(sar_u) {
 if (ARGC == N_(0)) GO(ret, Pn(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), >>); }
VM(sal_u) {
 if (ARGC == N_(0)) GO(ret, N_(0));
 TC(*ARGV, Num);
 mm_u(N(ARGC)-1, ARGV+1, N(*ARGV), <<); }

