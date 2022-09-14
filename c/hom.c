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

Vm(hnom_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Hom);
  return ApC(ret, hnom(v, xp)); }

// instructions for the internal compiler
Vm(hom_u) {
  ArityCheck(1);
  TypeCheck(xp = fp->argv[0], Num);
  size_t len = getnum(xp) + 2;
  Have(len);
  return
    xp = (ob) hp,
    hp += len,
    setw((ob*) xp, nil, len),
    ptr(xp)[len-2] = 0,
    ptr(xp)[len-1] = xp,
    ApC(ret, (ob) (ptr(xp) + len - 2)); }

Vm(hfin_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Hom);
  return
    button((mo)xp)[1].ll = (vm*) xp,
    ApC(ret, xp); }

Vm(emx) {
  mo k = (mo) *sp++ - 1;
  return k->ll = (vm*) xp,
         ApN(1, (ob) k); }

Vm(emi) {
  mo k = (mo) *sp++ - 1;
  return k->ll = (vm*) getZ(xp),
         ApN(1, (ob) k); }

Vm(emx_u) {
  ArityCheck(2);
  TypeCheck(xp = Argv[1], Hom);
  return
    xp = (ob) (ptr(xp) - 1),
    ptr(xp)[0] = fp->argv[0],
    ApC(ret, xp); }

Vm(emi_u) {
  ob n;
  ArityCheck(2);
  TypeCheck(n = Argv[0], Num);
  TypeCheck(xp = Argv[1], Hom);
  return
    xp = (ob) (ptr(xp) - 1),
    ptr(xp)[0] = getZ(n),
    ApC(ret, xp); }

Vm(peeki_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Hom);
  return ApC(ret, putnum(gethom(xp)->ll)); }

Vm(peekx_u) {
  ArityCheck(1);
  TypeCheck(xp = Argv[0], Hom);
  return ApC(ret, (ob) gethom(xp)->ll); }

Vm(seek_u) {
  ArityCheck(2);
  TypeCheck(xp = Argv[0], Hom);
  TypeCheck(Argv[1], Num);
  return ApC(ret, xp + Argv[1] - Num); }
