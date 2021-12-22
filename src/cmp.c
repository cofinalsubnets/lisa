#include "lips.h"
#include "cmp.h"
#include "two.h"
#include "hom.h"
#include "terp.h"

static bool eql_two(obj a, obj b) {
  // pairs are immutable, so we can deduplicate their insides.
  if (eql(A(a), A(b))) {
    A(b) = A(a);
    if (eql(B(a), B(b))) {
      B(b) = B(a);
      return true; } }
  return false; }

static bool eql_str(obj a, obj b) {
      str o = S(a), m = S(b);
      if (o->len != m->len) return false;
      return scmp(o->text, m->text) == 0; }

bool eql(obj a, obj b) {
  if (a == b) return true;
  if (kind(a) != kind(b)) return false;
  switch (kind(a)) {
    default: return false;
    case Two: return eql_two(a, b);
    case Str: return eql_str(a, b); } }

#define cmp_(n, op) BINOP(n, *sp++ op xp ? xp : nil)
cmp_(lt, <) cmp_(lteq, <=) cmp_(gteq, >=) cmp_(gt, >)
// there should be a separate instruction for simple equality?
BINOP(eq, eql(xp, *sp++) ? ok : nil)

static VM(ord_) {
  bool (*r)(obj, obj) = (void*)v->xp;
  obj n = N(ARGC), *xs = ARGV, m, *l;
  switch (n) {
    case 0: no: GO(ret, nil);
    default:
      for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)
        if (!r(m, xs[1])) goto no;
    case 1: break;}
  GO(ret, ok);}

#define ord_w(r)v->xp=(obj)r;Jump(ord_)
#define cmp(op, n)\
 static bool cmp_##n(obj a, obj b) { return a op b; }\
 VM(n##_u) { v->xp=(obj)cmp_##n;Jump(ord_); }
cmp(<, lt) cmp(<=, lteq) cmp(>=, gteq) cmp(>, gt)
VM(eq_u) { ord_w(eql); }
