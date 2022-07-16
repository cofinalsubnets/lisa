#include "la.h"

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

#define UBINOP(nom, dflt, op) Ll(nom##_u) {\
  mm_u(getZ(fp->argc), fp->argv, dflt, op); }

Ll(sub_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  if (xp == 1) return ApC(ret, putZ(-getZ(*fp->argv)));
  mm_u(xp-1, fp->argv+1, getZ(*fp->argv), -); }

Ll(sar_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getZ(fp->argc)-1, fp->argv+1, getZ(fp->argv[0]), >>); }

Ll(sal_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(fp->argv[0], Num);
  mm_u(getZ(fp->argc)-1, fp->argv+1, getZ(fp->argv[0]), <<); }

Ll(dqv) {
  if (xp == putZ(0)) return ApC(dom_err, xp);
  xp = putZ(getZ(*sp++) / getZ(xp));
  return ApN(1, xp); }

Ll(div_u) {
  if (!(xp = getZ(fp->argc))) return ApC(ret, T);
  TypeCheck(fp->argv[0], Num);
  mm_void(xp-1, fp->argv+1, getZ(fp->argv[0]), /); }

Ll(mod) {
  if (xp == putZ(0)) return ApC(dom_err, xp);
  xp = putZ(getZ(*sp++) % getZ(xp));
  return ApN(1, xp); }

Ll(mod_u) {
  if (!(xp = getZ(fp->argc))) return ApC(ret, T);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getZ(*fp->argv), %); }

Ll(rnd_u) {
  v->rand = lcprng(v->rand);
  xp = putZ(v->rand);
  return ApC(ret, xp); }

Op(1, neg, putZ(-getZ(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  putZ(getZ(*sp++) * getZ(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  putZ(getZ(*sp++) >> getZ(xp)))
BINOP(sal,  putZ(getZ(*sp++) << getZ(xp)))
UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)

Ll(bnot_u) {
  ArityCheck(1);
  return ApC(ret, putZ(~getZ(*fp->argv))); }

intptr_t lcprng(intptr_t s) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }
