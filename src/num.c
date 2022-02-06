#include "lips.h"
#include "terp.h"
#include "hom.h"

#define div0_err_msg "%d / 0"

#define mm_u(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op N(x)){\
  x = *xs++; Tc(x, Num);}\
 Go(ret, _N(xp));}

#define mm_u0(_c,_v,_z,op){\
 obj x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op N(x)){\
  x = *xs++; Tc(x, Num);\
  if (x == _N(0)) Jump(nope, div0_err_msg, xp);}\
 Go(ret, _N(xp));}

Vm(sub_u) {
  if (!(xp = N(Argc))) Go(ret, _N(0));
  Tc(*Argv, Num);
  if (xp == 1) Go(ret, _N(-N(*Argv)));
  mm_u(xp-1,Argv+1,N(*Argv),-); }

Vm(sar_u) {
  if (Argc == _N(0)) Go(ret, _N(0));
  Tc(*Argv, Num);
  mm_u(N(Argc)-1, Argv+1, N(*Argv), >>); }

Vm(sal_u) {
  if (Argc == _N(0)) Go(ret, _N(0));
  Tc(*Argv, Num);
  mm_u(N(Argc)-1, Argv+1, N(*Argv), <<); }

Vm(dqv) {
  if (xp == _N(0)) Jump(nope, div0_err_msg, N(*sp));
  xp = _N(N(*sp++) / N(xp));
  Next(1); }

Vm(div_u) {
  if (!(xp = N(Argc))) Go(ret, ok);
  Tc(*Argv, Num);
  mm_u0(xp-1,Argv+1,N(*Argv),/); }

Vm(mod) {
  if (xp == _N(0))
    Jump(nope, div0_err_msg, N(*sp));
  xp = _N(N(*sp++) % N(xp));
  Next(1); }

Vm(mod_u) {
  if (!(xp = N(Argc))) Go(ret, ok);
  Tc(*Argv, Num);
  mm_u0(xp-1,Argv+1,N(*Argv),%); }

Vm(rnd_u) {
  Go(ret, _N(lcprng(&v->rand))); }

OP1(neg, _N(-N(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  _N(N(*sp++)  * N(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  _N(N(*sp++) >> N(xp)))
BINOP(sal,  _N(N(*sp++) << N(xp)))

#define UBINOP(nom, dflt, op) Vm(nom##_u) { mm_u(N(Argc), Argv, dflt, op); }

UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)

u0 emnum(lips v, FILE *o, obj x) {
  fprintf(o, "%ld", (long) N(x)); }
