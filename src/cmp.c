#include "la.h"
#include "vm.h"
#include <string.h>

// isolate the more complicated logic from the simple
// pointer comparisons so eql() doesn't touch the stack
// unless it has to
static NoInline bool eql_two(la v, two a, two b) {
  return eql(v, a->a, b->a) ? eql(v, a->b, b->b) : false; }

static NoInline bool eql_str(str a, str b) {
  return a->len == b->len && 0 == scmp(a->text, b->text); }

static NoInline bool eql_(la v, ob a, ob b) {
  if (G(a) != disp || G(b) != disp || GF(a) != GF(b)) return false;
  if (GF(a) == (vm*) mtbl_two) return eql_two(v, (two) a, (two) b);
  if (GF(a) == (vm*) mtbl_str) return eql_str((str) a, (str) b);
  return false; }

bool eql(la v, ob a, ob b) {
  return a == b ? true : nump(a|b) ? false : eql_(v, a, b); }

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
  ob n = getnum(fp->argc), *xs = fp->argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, T); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(EQ, eq)

// type predicates
#define Tp(t)\
  Vm(t##pp) { return ApN(1, (t##p(xp)?T:nil)); }\
  Vm(t##p_u) {\
    for (ob *xs = fp->argv, *l = xs + getnum(fp->argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// type/arity checking
Vm(idZ) { return nump(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(idH) { return homp(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(idT) { return tblp(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(id2) { return twop(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(arity) {
  ob reqd = (ob) GF(ip);
  return fp->argc >= reqd ? ApN(2, xp) : ApC(ary_err, reqd); }
