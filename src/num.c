#include "lisa.h"
#include "vm.h"

// FIXME remove macros
#define mm_u(_c,_v,_z,op) {\
  ob x, *xs = _v, *l = xs + _c;\
  for (xp = _z; xs < l; xp = xp op getnum(x))\
    Check(nump(x = *xs++));\
  return ApC(ret, putnum(xp)); }

Vm(sub_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, N0);
  Check(nump(*Argv));
  if (xp == 1) return ApC(ret, putnum(-getnum(*Argv)));
  mm_u(xp-1, Argv+1, getnum(*Argv), -); }

Vm(sar_u) {
  if (Argc == N0) return ApC(ret, N0);
  Check(nump(Argv[0]));
  mm_u(getnum(Argc)-1, Argv+1, getnum(Argv[0]), >>); }

Vm(sal_u) {
  if (Argc == N0) return ApC(ret, N0);
  Check(nump(Argv[0]));
  mm_u(getnum(Argc)-1, Argv+1, getnum(Argv[0]), <<); }

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
    if (x == N0) return ApC(dom_err, x);}\
  return ApC(ret, putnum(xp));}

Vm(div_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, T);
  Check(nump(Argv[0]));
  mm_void(xp-1, Argv+1, getnum(Argv[0]), /); }

Vm(mod_u) {
  if (!(xp = getnum(Argc))) return ApC(ret, T);
  Check(nump(*Argv));
  mm_void(xp-1, Argv+1, getnum(*Argv), %); }

Vm(rnd_u) { return
  xp = lcprng(v->rand),
  v->rand = xp,
  ApC(ret, putnum(xp)); }

Vm(neg) { return ApN(1, putnum(-getnum(xp))); }

Vm(bnot_u) {
  ArityCheck(1);
  return ApC(ret, putnum(~getnum(*Argv))); }

Vm(add_u) { mm_u(getnum(Argc), Argv, 0, +); }
Vm(bor_u) { mm_u(getnum(Argc), Argv, 0, |); }
Vm(bxor_u) { mm_u(getnum(Argc), Argv, 0, ^); }
Vm(mul_u) { mm_u(getnum(Argc), Argv, 1, *); }
Vm(band_u) { mm_u(getnum(Argc), Argv, -1, &); }

Vm(add) { xp = xp + *sp++ - Num; return ApN(1, xp); }
Vm(sub) { xp = *sp++ - xp + Num; return ApN(1, xp); }

Vm(bor) { xp = xp | *sp++; return ApN(1, xp); }
Vm(band) { xp = xp & *sp++; return ApN(1, xp); }
Vm(bxor) { xp = (xp ^ *sp++) | Num; return ApN(1, xp); }

Vm(mul) {
  xp = putnum(getnum(*sp++) * getnum(xp));
  return ApN(1, xp); }
Vm(sar) {
  xp = putnum(getnum(*sp++) >> getnum(xp));
  return ApN(1, xp); }
Vm(sal) {
  xp = putnum(getnum(*sp++) << getnum(xp));
  return ApN(1, xp); }

bool nump(ob _) { return TypeOf(_) == Num; }
