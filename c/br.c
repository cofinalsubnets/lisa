#include "la.h"
#include <string.h>

bool eql(ob a, ob b) {
  return a == b || (Q(a) == Q(b) &&
    ((twop(a) && eql(A(a), A(b)) && eql(B(a), B(b))) ||
     (strp(a) && getstr(a)->len == getstr(b)->len &&
      0 == strcmp(getstr(a)->text, getstr(b)->text)))); }

#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)

BINOP(eq, eql(xp, *sp++) ? T : nil)
#define cmp(n, op) BINOP(n, op(*sp++, xp) ? xp : nil)
cmp(lt, LT) cmp(lteq, LE) cmp(gteq, GE) cmp(gt, GT)
#undef cmp
#define cmp(op, n) Ll(n##_u) {\
  ob n = getnum(fp->argc), *xs = fp->argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, T); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)

// type predicates
#define Tp(t)\
  Ll(t##pp) { return ApN(1, (t##p(xp)?T:nil)); }\
  Ll(t##p_u) {\
    for (ob *xs = fp->argv, *l = xs + getnum(fp->argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Ll(nom) {\
  return ApY(test ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq, eql(*sp++, xp), GF, T, FF, nil)
Br(brne, eql(*sp++, xp), FF, T, GF, nil)

Br(brlt,    *sp++ < xp,  GF, xp, FF, nil)
Br(brlt2,   *sp++ < xp,  FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ > xp,  GF, xp, FF, nil)
Br(brgt2,   *sp++ > xp,  FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt

