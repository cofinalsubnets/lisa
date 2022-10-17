#include "lisa.h"
#include "vm.h"

// FIXME remove macros
#define mm_u(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);}\
  return ApC(ret, putnum(xp));}

Vm(sub_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  if (xp == 1) return ApC(ret, putnum(-getnum(*fp->argv)));
  mm_u(xp-1, fp->argv+1, getnum(*fp->argv), -); }

Vm(sar_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(fp->argv[0]), >>); }

Vm(sal_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(fp->argv[0]), <<); }

Vm(dqv) {
  if (xp == putnum(0)) return ApC(dom_err, xp);
  xp = putnum(getnum(*sp++) / getnum(xp));
  return ApN(1, xp); }

Vm(mod) {
  if (xp == putnum(0)) return ApC(dom_err, xp);
  xp = putnum(getnum(*sp++) % getnum(xp));
  return ApN(1, xp); }

#define mm_void(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);\
    if (x == N0) return ApC(dom_err, x);}\
  return ApC(ret, putnum(xp));}

Vm(div_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  TypeCheck(fp->argv[0], Num);
  mm_void(xp-1, fp->argv+1, getnum(fp->argv[0]), /); }

Vm(mod_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getnum(*fp->argv), %); }

Vm(rnd_u) { return
  xp = lcprng(v->rand),
  v->rand = xp,
  ApC(ret, putnum(xp)); }

Vm(neg) { return ApN(1, putnum(-getnum(xp))); }

Vm(bnot_u) {
  ArityCheck(1);
  return ApC(ret, putnum(~getnum(*fp->argv))); }

Vm(add_u) { mm_u(getnum(fp->argc), fp->argv, 0, +); }
Vm(bor_u) { mm_u(getnum(fp->argc), fp->argv, 0, |); }
Vm(bxor_u) { mm_u(getnum(fp->argc), fp->argv, 0, ^); }
Vm(mul_u) { mm_u(getnum(fp->argc), fp->argv, 1, *); }
Vm(band_u) { mm_u(getnum(fp->argc), fp->argv, -1, &); }

Vm(add) { xp = xp + *sp++ - Num; return ApN(1, xp); }
Vm(sub) { xp = *sp++ - xp + Num; return ApN(1, xp); }

Vm(bor) { xp = xp | *sp++; return ApN(1, xp); }
Vm(band) { xp = xp & *sp++; return ApN(1, xp); }
Vm(bxor) { xp = (xp ^ *sp++) | Num; return ApN(1, xp); }

Vm(mul) { xp = putnum(getnum(*sp++) * getnum(xp)); return ApN(1, xp); }
Vm(sar) { xp = putnum(getnum(*sp++) >> getnum(xp)); return ApN(1, xp); }
Vm(sal) { xp = putnum(getnum(*sp++) << getnum(xp)); return ApN(1, xp); }
