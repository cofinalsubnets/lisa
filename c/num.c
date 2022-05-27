#include "la.h"

#define mm_u(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);}\
  return ApC(ret, putnum(xp));}

#define mm_void(_c,_v,_z,op){\
  ob x,*xs=_v,*l=xs+_c;\
  for(xp=_z;xs<l;xp=xp op getnum(x)){\
    x = *xs++;\
    TypeCheck(x, Num);\
    if (x == N0) return ApC(domain_error, x);}\
  return ApC(ret, putnum(xp));}

#define UBINOP(nom, dflt, op) Ll(nom##_u) {\
  mm_u(getnum(fp->argc), fp->argv, dflt, op); }

Ll(sub_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  if (xp == 1) return ApC(ret, putnum(-getnum(*fp->argv)));
  mm_u(xp-1,fp->argv+1,getnum(*fp->argv),-); }

Ll(sar_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(*fp->argv), >>); }

Ll(sal_u) {
  if (fp->argc == N0) return ApC(ret, N0);
  TypeCheck(*fp->argv, Num);
  mm_u(getnum(fp->argc)-1, fp->argv+1, getnum(*fp->argv), <<); }

Ll(dqv) {
  return xp == N0 ? ApC(domain_error, xp) :
    (xp = putnum(getnum(*sp++) / getnum(xp)), ApN(1, xp)); }

Ll(div_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getnum(*fp->argv), /); }

Ll(mod) {
  return xp == N0 ? ApC(domain_error, xp) :
    (xp = putnum(getnum(*sp++) % getnum(xp)), ApN(1, xp)); }

Ll(mod_u) {
  if (!(xp = getnum(fp->argc))) return ApC(ret, T);
  TypeCheck(*fp->argv, Num);
  mm_void(xp-1, fp->argv+1, getnum(*fp->argv), %); }

Ll(rnd_u) { return
  xp = putnum(v->rand = lcprng(v->rand)), ApC(ret, xp); }

Op(1, neg, putnum(-getnum(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  putnum(getnum(*sp++) * getnum(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  putnum(getnum(*sp++) >> getnum(xp)))
BINOP(sal,  putnum(getnum(*sp++) << getnum(xp)))
UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)

Ll(bnot_u) {
  Arity(1);
  return
    xp = fp->argv[0],
    ApC(ret, putnum(~getnum(xp))); }
