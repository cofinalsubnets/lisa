#include "i.h"

bool neql(la v, ob x, ob y) { return false; }
bool _eql(la v, ob a, ob b) { return
  (!nump(a|b) && G(a) == act &&
   ((typ) GF(a))->equi(v, a, b)); }

// comparison operators
Vm(lt) { return ApN(1, *sp++ < xp ? T : nil); }
Vm(lteq) { return ApN(1, *sp++ <= xp ? T : nil); }
Vm(eq) { return ApN(1, eql(v, *sp++, xp) ? T : nil); }
Vm(gteq) { return ApN(1, *sp++ >= xp ? T : nil); }
Vm(gt) { return ApN(1, *sp++ > xp ? T : nil); }

// TODO remove macros
#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
#define EQ(a,b) eql(v,a,b)
#define cmp(op, n) Vm(n##_f) {\
  for (I i = fp->argc-1; i > 0; i--)\
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

Vm(vm_yield) {
  enum status r = v->xp;
  return Pack(), r; }
// type/arity checking
Vm(idno) { return nump(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idmo) { return homp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtbl) { return tblp(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(idtwo) { return twop(xp) ? ApN(1, xp) : ApC(xdom, xp); }
Vm(arity) { return
  fp->argc >= getnum(GF(ip)) ? ApN(2, xp) :
    Yield(ArityError, (ob) GF(ip)); }
Vm(ary1) { return
  fp->argc ? ApN(1, xp) :
    Yield(ArityError, putnum(1)); }
Vm(ary2) { return
  fp->argc >= 2 ? ApN(1, xp) :
    Yield(ArityError, putnum(2)); }
Vm(ary3) { return
  fp->argc >= 3 ? ApN(1, xp) :
    Yield(ArityError, putnum(3)); }
Vm(ary4) { return
  fp->argc >= 4 ? ApN(1, xp) :
    Yield(ArityError, putnum(4)); }
