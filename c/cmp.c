#include "lips.h"
#include "cmp.h"
#include "two.h"
#include "str.h"
#include "num.h"
#include <string.h>

NoInline u1 eql_(obj a, obj b) { return eql(a, b); }

static NoInline u1 eql_two(obj a, obj b) {
  return eql_(A(a), A(b)) && eql_(B(a), B(b)); }

static NoInline u1 eql_str(obj a, obj b) {
  return S(a)->len == S(b)->len &&
    strcmp(S(a)->text, S(b)->text) == 0; }

Inline u1 eql(obj a, obj b) {
  if (a == b) return true;
  if (kind(a) != kind(b)) return false;
  switch (kind(a)) { case Two: return eql_two(a, b);
                     case Str: return eql_str(a, b);
                     default: return false; } }

#include "hom.h"
#include "terp.h"
#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
BINOP(eq, eql(xp, *sp++) ? ok : nil)
#define cmp(n, op) BINOP(n, op(*sp++, xp) ? xp : nil)
cmp(lt, LT) cmp(lteq, LE) cmp(gteq, GE) cmp(gt, GT)
#undef cmp
#define cmp(op, n) Vm(n##_u) {\
  obj n = N(Argc), *xs = Argv, m, *l;\
  switch (n) {\
    case 0: Go(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) Go(ret, nil);\
    case 1: Go(ret, ok); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)
