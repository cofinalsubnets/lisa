#include "lips.h"
#include "terp.h"

////
/// Load Instructions
//
// constants
OP1(unit, nil)
OP1(one, _N(1))
OP1(zero, _N(0))
// immediate value from thread
OP2(imm, (obj) H(ip)[1])

// indexed references
#define REF(b) (*(i64*)((i64)(b)+(i64)H(ip)[1]-Num))
// pointer arithmetic works because fixnums are premultiplied by W

// function arguments
OP2(arg, REF(Argv))
OP1(arg0, Argv[0])
OP1(arg1, Argv[1])
// local variables
OP2(loc, REF(V(Locs)->xs))
OP1(loc0, V(Locs)->xs[0])
OP1(loc1, V(Locs)->xs[1])
// closure variables
OP2(clo, REF(V(Clos)->xs))
OP1(clo0, V(Clos)->xs[0])
OP1(clo1, V(Clos)->xs[1])

////
/// Store Instructions
//
// stack push
Vm(push) { Have1(); *--sp = xp; Next(1); }
// set a local variable
Vm(loc_) { REF(V(Locs)->xs) = xp; Next(2); }
// set a global variable
Vm(tbind) { CallC(tbl_set(v, Top, (obj) H(ip)[1], xp)); Next(2); }

// allocate local variable array
Vm(locals) {
 i64 n = N((i64) H(ip)[1]);
 Have(n + 2);
 vec t = (vec) hp;
 set64(t->xs, nil, t->len = n);
 hp += n + 1;
 *--sp = _V(t);
 Next(2); }

// late binding
// long b/c it does the "static" type and arity checks
// that would have been done by the compiler if the function
// had been bound early.
Vm(lbind) {
  obj w = (obj) H(ip)[1], d = AB(w), y = A(w);
  if (!(w = tbl_get(v, d, xp = BB(w)))) {
    char *nom = nilp(Y(xp)->nom) ? "()" : S(Y(xp)->nom)->text;
    return Pack(), err(v, "free variable : %s", nom); }
  xp = w;
  if (y != _N(8)) Tc(xp, N(y)); // do the type check
  terp *q = H(ip)[2]; // omit the arity check if possible
  if (q == call || q == rec) {
    obj aa = (obj) H(ip)[3];
    if (H(xp)[0] == arity && aa >= (obj) H(xp)[1]) xp += word * 2; }
  H(ip)[0] = imm;
  H(ip)[1] = (terp*) xp;
  Next(2); }
