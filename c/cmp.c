#include "lips.h"
#include "terp.h"
#include <string.h>

static NoInline u1 eql_(ob a, ob b) { return eql(a, b); }

static NoInline u1 eql_two(two a, two b) {
  return eql_(a->a, b->a) && eql_(a->b, b->b); }

static NoInline u1 eql_str(str a, str b) {
  return a->len == b->len &&
    strcmp(a->text, b->text) == 0; }

Inline u1 eql(ob a, ob b) {
  return a == b || (Q(a) == Q(b) &&
    ((twop(a) && eql_two(gettwo(a), gettwo(b))) ||
     (strp(a) && eql_str(getstr(a), getstr(b))))); }

#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
BINOP(eq, eql(xp, *sp++) ? ok : nil)
#define cmp(n, op) BINOP(n, op(*sp++, xp) ? xp : nil)
cmp(lt, LT) cmp(lteq, LE) cmp(gteq, GE) cmp(gt, GT)
#undef cmp
#define cmp(op, n) Vm(n##_u) {\
  ob n = N(Argc), *xs = Argv, m, *l;\
  switch (n) {\
    case 0: Go(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) Go(ret, nil);\
    case 1: Go(ret, ok); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)
