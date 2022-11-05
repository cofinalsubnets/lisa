#include "la.h"

bool neql(la v, ob x, ob y) { return false; }

bool eql(la v, ob a, ob b) {
  return a == b || (!nump(a|b) && G(a) == disp &&
    ((mtbl) GF(a))->equi(v, a, b)); }

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
#define cmp(op, n) Vm(n##_f) {\
  for (long i = fp->argc-1; i > 0; i--)\
    if (!op(fp->argv[i-1], fp->argv[i])) return ApC(ret, nil);\
  return ApC(ret, T); }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(EQ, eq)

// type predicates
#define Tp(t)\
  Vm(t##p_) { return ApN(1, (t##p(xp)?T:nil)); }\
  Vm(t##p_f) {\
    for (size_t i = fp->argc; i;)\
      if (!t##p(fp->argv[--i])) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// type/arity checking
Vm(idno) { return nump(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idmo) { return homp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtbl) { return tblp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtwo) { return twop(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(arity) {
  ob reqd = getnum(GF(ip));
  ArityCheck(reqd);
  return ApN(2, xp); }
