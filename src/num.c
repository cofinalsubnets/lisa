#include "la.h"
#include "vm.h"

// FIXME remove macros
#define mm_u(_c,_v,_z,op) {\
  ob x, *xs = _v, *l = xs + _c;\
  for (xp = _z; xs < l; xp = xp op getnum(x))\
    Check(nump(x = *xs++));\
  return ApC(ret, putnum(xp)); }

Vm(sub_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, putnum(0));
  Check(nump(*fp->argv));
  if (xp == 1) return ApC(ret, putnum(-getnum(*fp->argv)));
  mm_u(xp-1, fp->argv+1, getnum(*fp->argv), -); }

Vm(sar_u) {
  if (fp->argc == putnum(0)) return ApC(ret, putnum(0));
  Check(nump(fp->argv[0]));
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(fp->argv[0]), >>); }

Vm(sal_u) {
  if (fp->argc == putnum(0)) return ApC(ret, putnum(0));
  Check(nump(fp->argv[0]));
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
    Check(nump(x));\
    if (x == putnum(0)) return ApC(dom_err, x);}\
  return ApC(ret, putnum(xp));}

Vm(div_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  Check(nump(fp->argv[0]));
  mm_void(xp-1, fp->argv+1, getnum(fp->argv[0]), /); }

Vm(mod_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  Check(nump(fp->argv[0]));
  mm_void(xp-1, fp->argv+1, getnum(fp->argv[0]), %); }

Vm(rnd_u) {
  xp = v->rand = lcprng(v->rand);
  return ApC(ret, putnum(xp)); }

Vm(neg) { return ApN(1, putnum(-getnum(xp))); }

Vm(bnot_u) {
  ArityCheck(1);
  return ApC(ret, ~*fp->argv|1); }

Vm(add_u) { mm_u(getnum(fp->argc), fp->argv, 0, +); }
Vm(bor_u) { mm_u(getnum(fp->argc), fp->argv, 0, |); }
Vm(bxor_u) { mm_u(getnum(fp->argc), fp->argv, 0, ^); }
Vm(mul_u) { mm_u(getnum(fp->argc), fp->argv, 1, *); }
Vm(band_u) { mm_u(getnum(fp->argc), fp->argv, -1, &); }

Vm(add) { xp = xp + *sp++ - 1; return ApN(1, xp); }
Vm(sub) { xp = *sp++ - xp + 1; return ApN(1, xp); }

Vm(bor) { xp = xp | *sp++; return ApN(1, xp); }
Vm(band) { xp = xp & *sp++; return ApN(1, xp); }
Vm(bxor) { xp = (xp ^ *sp++) | 1; return ApN(1, xp); }

Vm(mul) {
  xp = putnum(getnum(*sp++) * getnum(xp));
  return ApN(1, xp); }
Vm(sar) {
  xp = putnum(getnum(*sp++) >> getnum(xp));
  return ApN(1, xp); }
Vm(sal) {
  xp = putnum(getnum(*sp++) << getnum(xp));
  return ApN(1, xp); }
