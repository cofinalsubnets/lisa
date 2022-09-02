#include "la.h"

mo button(mo k) { return G(k) ? button(F(k)) : k; }

#include "vm.h"
ob hnom(la v, ob x) {
  vm *k = gethom(x)->ll;
  if (k == clos || k == clos0 || k == clos1)
    return hnom(v, (ob) gethom(x)[2].ll);
  ob* h = (ob*) gethom(x);
  while (*h) h++;
  x = h[-1];
  int inb = (ob*) x >= v->pool && (ob*) x < v->pool+v->len;
  return inb ? x : nil; }

Vm(hnom_u) { return
  Arity == 0 ? ArityError(1) :
  TypeOf(xp = fp->argv[0]) != Hom ? Undefined() :
  ApC(ret, hnom(v, xp)); }

// instructions for the internal compiler
Vm(hom_u) {
  size_t len; return
    Arity == 0 ? ArityError(1) :
    TypeOf(xp = fp->argv[0]) != Num ? Undefined() :
    Free < (len = getZ(xp) + 2) ? Collect(len) :
    (xp = (ob) hp,
     hp += len,
     setw((ob*) xp, nil, len),
     ptr(xp)[len-2] = 0,
     ptr(xp)[len-1] = xp,
     ApC(ret, (ob) (ptr(xp) + len - 2))); }

Vm(hfin_u) { return
  Arity == 0 ? ArityError(1) :
  TypeOf(xp = fp->argv[0]) != Hom ? Undefined() :
  (button((mo)xp)[1].ll = (vm*) xp,
   ApC(ret, xp)); }

Vm(emx) {
  mo k = (mo) *sp++ - 1;
  return k->ll = (vm*) xp,
         ApN(1, (ob) k); }

Vm(emi) {
  mo k = (mo) *sp++ - 1;
  return k->ll = (vm*) getZ(xp),
         ApN(1, (ob) k); }

Vm(emx_u) { return
  Arity < 2 ? ArityError(2) :
  TypeOf(xp = fp->argv[1]) != Hom ? Undefined() :
  (xp = (ob) (ptr(xp) - 1),
   ptr(xp)[0] = fp->argv[0],
   ApC(ret, xp)); }

Vm(emi_u) { ob n; return
  Arity < 2 ? ArityError(2) :
  TypeOf(n = fp->argv[0]) != Num ||
  TypeOf(xp = fp->argv[1]) != Hom ? Undefined() :
  (xp = (ob) (ptr(xp) - 1),
   ptr(xp)[0] = getZ(n),
   ApC(ret, xp)); }

Vm(peeki_u) { return
  Arity == 0 ? ArityError(1) :
  TypeOf(xp = fp->argv[0]) != Hom ? Undefined() :
  ApC(ret, putZ(gethom(xp)->ll)); }

Vm(peekx_u) { return
  Arity == 0 ? ArityError(1) :
  TypeOf(xp = fp->argv[0]) != Hom ? Undefined() :
  ApC(ret, (ob) gethom(xp)->ll); }

Vm(seek_u) { return
  Arity < 2 ? ArityError(2) :
  TypeOf(xp = fp->argv[0]) != Hom ||
  TypeOf(fp->argv[1]) != Num ? Undefined() :
  ApC(ret, xp + fp->argv[1] - Num); }
