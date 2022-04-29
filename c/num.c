#include "lips.h"
#include "terp.h"

#define mm_u(_c,_v,_z,op){\
 ob x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op N(x)){\
  x = *xs++; Tc(x, Num);}\
 Go(ret, _N(xp));}

#define mm_u0(_c,_v,_z,op){\
 ob x,*xs=_v,*l=xs+_c;\
 for(xp=_z;xs<l;xp=xp op N(x)){\
  x = *xs++; Tc(x, Num);\
  if (x == _N(0)) Jump(div_error);}\
 Go(ret, _N(xp));}

Vm(sub_u) {
  if (!(xp = N(Argc))) Go(ret, _N(0));
  TypeCheck(*Argv, Num);
  if (xp == 1) Go(ret, _N(-N(*Argv)));
  mm_u(xp-1,Argv+1,N(*Argv),-); }

Vm(sar_u) {
  if (Argc == _N(0)) Go(ret, _N(0));
  TypeCheck(*Argv, Num);
  mm_u(N(Argc)-1, Argv+1, N(*Argv), >>); }

Vm(sal_u) {
  if (Argc == _N(0)) Go(ret, _N(0));
  TypeCheck(*Argv, Num);
  mm_u(N(Argc)-1, Argv+1, N(*Argv), <<); }

Vm(dqv) {
  if (xp == _N(0)) Jump(div_error);
  xp = _N(N(*sp++) / N(xp));
  Next(1); }

Vm(div_u) {
  if (!(xp = N(Argc))) Go(ret, ok);
  Tc(*Argv, Num);
  mm_u0(xp-1,Argv+1,N(*Argv),/); }

Vm(mod) {
  if (xp == _N(0)) Jump(div_error);
  xp = _N(N(*sp++) % N(xp));
  Next(1); }

Vm(mod_u) {
  if (!(xp = N(Argc))) Go(ret, ok);
  Tc(*Argv, Num);
  mm_u0(xp-1,Argv+1,N(*Argv),%); }

Vm(rnd_u) { Go(ret, _N(v->rand = lcprng(v->rand))); }

OP1(neg, _N(-N(xp)))
BINOP(add,  xp + *sp++ - Num)
BINOP(bor,  xp | *sp++)
BINOP(bxor, (xp ^ *sp++) | Num)
BINOP(mul,  _N(N(*sp++)  * N(xp)))
BINOP(band, xp & *sp++)
BINOP(sub,  *sp++ - xp + Num)
BINOP(sar,  _N(N(*sp++) >> N(xp)))
BINOP(sal,  _N(N(*sp++) << N(xp)))

#define UBINOP(nom, dflt, op) Vm(nom##_u) { mm_u(N(Argc), Argv, dflt, op); }

UBINOP(add, 0, +)
UBINOP(bor, 0, |)
UBINOP(bxor, 0, ^)
UBINOP(mul, 1, *)
UBINOP(band, -1, &)
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
