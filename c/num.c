#include "la.h"
#include "vm.h"

#define BINOP(nom, xpn) Vm(nom) { xp = (xpn); return ApN(1, xp); }
// FIXME remove macros
#define mm_u(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getZ(x)){\
    x = *xs++;\
    TypeCheck(x, Num);}\
  return ApC(ret, putZ(xp));}

#define mm_void(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getZ(x)){\
    x = *xs++;\
    TypeCheck(x, Num);\
    if (x == N0) return ApC(dom_err, x);}\
  return ApC(ret, putZ(xp));}

Vm(sub_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  if (xp == 1) return ApC(ret, putZ(-getZ(*fp->argv)));
  mm_u(xp-1, fp->argv+1, getZ(*fp->argv), -); }

Vm(sar_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getZ(fp->argc)-1, fp->argv+1, getZ(fp->argv[0]), >>); }

Vm(sal_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getZ(fp->argc)-1, fp->argv+1, getZ(fp->argv[0]), <<); }

Vm(dqv) {
  if (xp == putZ(0)) return ApC(dom_err, xp);
  xp = putZ(getZ(*sp++) / getZ(xp));
  return ApN(1, xp); }

Vm(div_u) {
  if (!(xp = getZ(fp->argc))) return ApC(ret, T);
  TypeCheck(fp->argv[0], Num);
  mm_void(xp-1, fp->argv+1, getZ(fp->argv[0]), /); }

Vm(mod) {
  if (xp == putZ(0)) return ApC(dom_err, xp);
  xp = putZ(getZ(*sp++) % getZ(xp));
  return ApN(1, xp); }

Vm(mod_u) {
  if (!(xp = getZ(fp->argc))) return ApC(ret, T);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getZ(*fp->argv), %); }

Vm(rnd_u) { return
  xp = lcprng(v->rand),
  v->rand = xp,
  ApC(ret, putZ(xp)); }

Vm(neg) { return ApN(1, putZ(-getZ(xp))); }

Vm(bnot_u) {
  ArityCheck(1);
  return ApC(ret, putZ(~getZ(*fp->argv))); }

Vm(add_u) { mm_u(getZ(fp->argc), fp->argv, 0, +); }
Vm(bor_u) { mm_u(getZ(fp->argc), fp->argv, 0, |); }
Vm(bxor_u) { mm_u(getZ(fp->argc), fp->argv, 0, ^); }
Vm(mul_u) { mm_u(getZ(fp->argc), fp->argv, 1, *); }
Vm(band_u) { mm_u(getZ(fp->argc), fp->argv, -1, &); }

BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  putZ(getZ(*sp++) * getZ(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  putZ(getZ(*sp++) >> getZ(xp)))
BINOP(sal,  putZ(getZ(*sp++) << getZ(xp)))
