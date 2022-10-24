#include "lisa.h"
#include "vm.h"

// function functions
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
//
// two easy potential optimizations are:
// - add a tail pointer to the start of the function,
//   so GC can find the head quickly (since often we
//   won't have an internal pointer)
// - tag the tail/head pointers instead of using a null
//   sentinel (but then the C compiler would need to
//   align functions)

// allocate a thread
mo mkmo(la v, size_t n) {
  mo k = cells(v, n+2);
  if (k) k[n].ll = 0, k[n+1].ll = (vm*) k;
  return k; }

// get the tag at the end of a function
mo button(mo k) { return G(k) ? button(F(k)) : k; }

// try to get the name of a function
ob hnom(la v, ob x) {
  vm *k = G(x);
  if (k == clos || k == clos0 || k == clos1)
    return hnom(v, (ob) G(FF(x)));
  ob* h = (ob*) x;
  while (*h) h++;
  x = h[-1];
  bool inb = (ob*) x >= v->pool && (ob*) x < v->pool + v->len;
  return inb ? x : nil; }

// instructions for the internal compiler
Vm(hom_u) {
  ArityCheck(1);
  xp = Argv[0];
  Check(nump(xp));
  size_t len = getnum(xp) + 2;
  Have(len);
  xp = (ob) hp;
  hp += len;
  setw((ob*) xp, nil, len);
  ((ob*) xp)[len-2] = 0;
  ((ob*) xp)[len-1] = xp;
  return ApC(ret, (ob) (ptr(xp) + len - 2)); }

Vm(hfin_u) {
  ArityCheck(1);
  xp = Argv[0];
  Check(homp(xp) && G(xp) != disp);
  GF(button((mo)xp)) = (vm*) xp;
  return ApC(ret, xp); }

Vm(emx) {
  mo k = (mo) *sp++ - 1;
  k->ll = (vm*) xp;
  return ApN(1, (ob) k); }

Vm(emi) {
  mo k = (mo) *sp++ - 1;
  k->ll = (vm*) getnum(xp);
  return ApN(1, (ob) k); }

Vm(emx_u) {
  ArityCheck(2);
  xp = Argv[1];
  Check(homp(xp));
  xp = (ob) (ptr(xp) - 1);
  ptr(xp)[0] = Argv[0];
  return ApC(ret, xp); }

Vm(emi_u) {
  ArityCheck(2);
  ob n = Argv[0];
  xp = Argv[1];
  Check(nump(n));
  Check(homp(xp));
  xp = (ob) (ptr(xp) - 1);
  ptr(xp)[0] = getnum(n);
  return ApC(ret, xp); }

Vm(peeki_u) {
  ArityCheck(1);
  xp = Argv[0];
  Check(homp(xp));
  return ApC(ret, putnum(G(xp))); }

Vm(peekx_u) {
  ArityCheck(1);
  xp = Argv[0];
  Check(homp(xp));
  return ApC(ret, (ob) G(xp)); }

Vm(seek_u) {
  ArityCheck(2);
  xp = Argv[0];
  Check(homp(xp));
  Check(nump(Argv[1]));
  return ApC(ret, (ob) ((mo) xp + getnum(Argv[1]))); }

bool homp(ob _) { return TypeOf(_) == Hom; }
