#include "lips.h"
#include "cmp.h"

typedef bool rel(obj, obj);
static rel eql_two, eql_str;

bool eql(obj a, obj b) {
  if (a == b) return true;
  if (kind(a) != kind(b)) return false;
  switch (kind(a)) {
    default: return false;
    case Two: return eql_two(a, b);
    case Str: return eql_str(a, b); } }

#include "two.h"
static bool eql_two(obj a, obj b) {
  // pairs are immutable, so we can deduplicate their insides.
  if (eql(A(a), A(b))) {
    gettwo(b)->a = gettwo(a)->a;
    if (eql(B(a), B(b))) {
      gettwo(b)->b = gettwo(a)->b;
      return true; } }
  return false; }

#include "str.h"
static bool eql_str(obj a, obj b) {
  str o = S(a), m = S(b);
  if (o->len != m->len) return false;
  return scmp(o->text, m->text) == 0; }

#include "hom.h"
#include "terp.h"
#define cmp_(n, op) BINOP(n, *sp++ op xp ? xp : nil)
cmp_(lt, <) cmp_(lteq, <=) cmp_(gteq, >=) cmp_(gt, >)
// there should be a separate instruction for simple equality?
BINOP(eq, eql(xp, *sp++) ? ok : nil)

static VM(ord_) {
  bool (*r)(obj, obj) = (void*)v->xp;
  obj n = N(Argc), *xs = Argv, m, *l;
  switch (n) {
    case 0:  GO(ret, nil);
    default:
      for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)
        if (!r(m, xs[1])) GO(ret, nil);
    case 1: GO(ret, ok); } }

#define ord_w(r)v->xp=(obj)r;Jump(ord_)
#define cmp(op, n)\
  static bool cmp_##n(obj a, obj b) { return a op b; }\
  VM(n##_u) { v->xp=(obj)cmp_##n;Jump(ord_); }
cmp(<, lt) cmp(<=, lteq) cmp(>=, gteq) cmp(>, gt)
VM(eq_u) { ord_w(eql); }
