#include "lips.h"
#include "terp.h"
#include "hom.h"

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

VM(sub_u) {
  if (!(xp = N(Argc))) GO(ret, _N(0));
  TC(*Argv, Num);
  if (xp == 1) GO(ret, _N(-N(*Argv)));
  mm_u(xp-1,Argv+1,N(*Argv),-); }

VM(sar_u) {
  if (Argc == _N(0)) GO(ret, _N(0));
  TC(*Argv, Num);
  mm_u(N(Argc)-1, Argv+1, N(*Argv), >>); }

VM(sal_u) {
  if (Argc == _N(0)) GO(ret, _N(0));
  TC(*Argv, Num);
  mm_u(N(Argc)-1, Argv+1, N(*Argv), <<); }

VM(dqv) {
  if (xp == _N(0)) Jump(nope, div0_err_msg, N(*sp));
  xp = _N(N(*sp++) / N(xp));
  NEXT(1); }

VM(div_u) {
  if (!(xp = N(Argc))) GO(ret, ok);
  TC(*Argv, Num);
  mm_u0(xp-1,Argv+1,N(*Argv),/); }

VM(mod) {
  if (xp == _N(0))
    Jump(nope, div0_err_msg, N(*sp));
  xp = _N(N(*sp++) % N(xp));
  NEXT(1); }

VM(mod_u) {
  if (!(xp = N(Argc))) GO(ret, ok);
  TC(*Argv, Num);
  mm_u0(xp-1,Argv+1,N(*Argv),%); }

VM(rnd_u) {
  GO(ret, _N(lcprng(&v->rand))); }

OP1(neg, _N(-N(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  _N(N(*sp++)  * N(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  _N(N(*sp++) >> N(xp)))
BINOP(sal,  _N(N(*sp++) << N(xp)))

#define UBINOP(nom, dflt, op) VM(nom##_u) { mm_u(N(Argc), Argv, dflt, op); }

UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)

u0 emnum(lips v, FILE *o, obj x) {
  fprintf(o, "%ld", (long) N(x)); }
