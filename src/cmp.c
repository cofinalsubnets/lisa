#include "lips.h"
#include "cmp.h"
#include "two.h"
#include "str.h"

static NoInline u1 eql_(obj a, obj b) {
  return eql(a, b); }

static NoInline u1 eql_two(obj a, obj b) {
  return eql_(A(a), A(b)) && eql_(B(a), B(b)); }

static NoInline u1 eql_str(obj a, obj b) {
  return S(a)->len == S(b)->len &&
    scmp(S(a)->text, S(b)->text) == 0; }

Inline u1 eql(obj a, obj b) {
  if (a == b) return true;
  if (kind(a) != kind(b)) return false;
  if (kind(a) == Two) return eql_two(a, b);
  if (kind(a) == Str) return eql_str(a, b);
  return false; }

#include "hom.h"
#include "terp.h"
#define cmp_(n, op) BINOP(n, *sp++ op xp ? xp : nil)
cmp_(lt, <) cmp_(lteq, <=) cmp_(gteq, >=) cmp_(gt, >)
// there should be a separate instruction for simple equality?
BINOP(eq, eql(xp, *sp++) ? ok : nil)

static Vm(ord_) {
  u1 (*r)(obj, obj) = (u0*)v->xp;
  obj n = N(Argc), *xs = Argv, m, *l;
  switch (n) {
    case 0:  Go(ret, nil);
    default:
      for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)
        if (!r(m, xs[1])) Go(ret, nil);
    case 1: Go(ret, ok); } }

#define ord_w(r)v->xp=(obj)r;Jump(ord_)
#define cmp(op, n)\
  static u1 cmp_##n(obj a, obj b) { return a op b; }\
  Vm(n##_u) { v->xp=(obj)cmp_##n;Jump(ord_); }
cmp(<, lt) cmp(<=, lteq) cmp(>=, gteq) cmp(>, gt)
Vm(eq_u) { ord_w(eql); }
