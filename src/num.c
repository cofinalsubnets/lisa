#include "la.h"
#include "vm.h"

// FIXME remove macros
#define mm_u(_c,_v,_z,op) {\
  ob x, *xs = _v, *l = xs + _c;\
  for (xp = _z; xs < l; xp = xp op getnum(x))\
    Check(nump(x = *xs++));\
  return ApC(ret, putnum(xp)); }

Vm(sub_u) {
  if (!(xp = fp->argc)) return ApC(ret, putnum(0));
  Check(nump(*fp->argv));
  if (xp == 1) return ApC(ret, putnum(-getnum(*fp->argv)));
  mm_u(xp-1, fp->argv+1, getnum(*fp->argv), -); }

Vm(sar_u) {
  if (fp->argc == 0) return ApC(ret, putnum(0));
  Check(nump(fp->argv[0]));
  mm_u(fp->argc-1, fp->argv+1, getnum(fp->argv[0]), >>); }

Vm(sal_u) {
  if (fp->argc == 0) return ApC(ret, putnum(0));
  Check(nump(fp->argv[0]));
  mm_u(fp->argc-1, fp->argv+1, getnum(fp->argv[0]), <<); }

Vm(dqv) { return xp == putnum(0) ?
  ApC(dom_err, xp) :
  ApN(1, putnum(getnum(*sp++) / getnum(xp))); }

Vm(mod) { return xp == putnum(0) ?
  ApC(dom_err, xp) :
  ApN(1, putnum(getnum(*sp++) % getnum(xp))); }

#define mm_void(_c, _v, _z, op) {\
  ob x, *xs = _v, *l = xs + _c;\
  for (xp = _z; xs < l; xp = xp op getnum(x)) {\
    Check(nump(x = *xs++));\
    if (x == putnum(0)) return ApC(dom_err, x);}\
  return ApC(ret, putnum(xp));}

Vm(div_u) {
  if (!(xp = fp->argc)) return ApC(ret, T);
  Check(nump(fp->argv[0]));
  mm_void(xp-1, fp->argv+1, getnum(fp->argv[0]), /); }

Vm(mod_u) {
  if (!(xp = fp->argc)) return ApC(ret, T);
  Check(nump(fp->argv[0]));
  mm_void(xp-1, fp->argv+1, getnum(fp->argv[0]), %); }

Vm(rnd_u) { return ApC(ret, putnum(v->rand = lcprng(v->rand))); }

Vm(neg) { return ApN(1, putnum(-getnum(xp))); }

Vm(bnot_u) {
  ArityCheck(1);
  return ApC(ret, ~*fp->argv|1); }

Vm(add_u) { mm_u(fp->argc, fp->argv, 0, +); }
Vm(bor_u) { mm_u(fp->argc, fp->argv, 0, |); }
Vm(bxor_u) { mm_u(fp->argc, fp->argv, 0, ^); }
Vm(mul_u) { mm_u(fp->argc, fp->argv, 1, *); }
Vm(band_u) { mm_u(fp->argc, fp->argv, -1, &); }

Vm(add) { return ApN(1, xp + *sp++ - 1); }
Vm(sub) { return ApN(1, *sp++ - xp + 1); }

Vm(bor) { return ApN(1, xp | *sp++); }
Vm(band) { return ApN(1, xp & *sp++); }
Vm(bxor) { return ApN(1, (xp ^ *sp++) | 1); }

Vm(mul) { return ApN(1, putnum(getnum(*sp++) * getnum(xp))); }
Vm(sar) { return ApN(1, putnum(getnum(*sp++) >> getnum(xp))); }
Vm(sal) { return ApN(1, putnum(getnum(*sp++) << getnum(xp))); }
