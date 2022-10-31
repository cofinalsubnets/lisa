#include "la.h"

bool eq_no(la v, ob x, ob y) { return false; }

// break it up into two functions to give the compiler
// a hint about how to inline, maybe
static bool eql_(la v, ob a, ob b) {
  return !nump(a|b) && G(a) == disp &&
    ((mtbl) GF(a))->equi(v, a, b); }
bool eql(la v, ob a, ob b) {
  return a == b || eql_(v, a, b); }

// comparison operators
// XXX FIXME returning xp is wrong now that 0 = nil
Vm(lt) { return ApN(1, *sp++ < xp ? xp : nil); }
Vm(lteq) { return ApN(1, *sp++ <= xp ? xp : nil); }
Vm(eq) { return ApN(1, eql(v, xp, *sp++) ? T : nil); }
Vm(gteq) { return ApN(1, *sp++ >= xp ? xp : nil); }
Vm(gt) { return ApN(1, *sp++ > xp ? xp : nil); }

// TODO remove macros
#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
#define EQ(a,b) eql(v,a,b)
#define cmp(op, n) Vm(n##_u) {\
  ob n = fp->argc, *xs = fp->argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m = *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, T); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(EQ, eq)

// type predicates
#define Tp(t)\
  Vm(t##pp) { return ApN(1, (t##p(xp)?T:nil)); }\
  Vm(t##p_u) {\
    for (ob *xs = fp->argv, *l = xs + fp->argc; xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// type/arity checking
Vm(idZ) { return nump(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idH) { return homp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idT) { return tblp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(id2) { return twop(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(arity) {
  ob reqd = getnum(GF(ip));
  ArityCheck(reqd);
  return ApN(2, xp); }
