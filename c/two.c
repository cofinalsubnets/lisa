#include "la.h"

// pairs and lists
ob pair(la v, ob a, ob b) {
  if (Avail < 2) {
    bool _;
    with(a, with(b, _ = please(v, 2)));
    if (!_) return 0; }
  two w = bump(v, 2);
  return w->a = a, w->b = b, puttwo(w); }

// length of list
size_t llen(ob l) {
  size_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

// vm functions
#include "vm.h"

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have1();
  hp[0] = xp;
  hp[1] = *sp++;
  xp = puttwo(hp);
  hp += 2;
  return ApN(1, xp); }

Vm(car_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, A(fp->argv[0])); }

Vm(cdr_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, B(fp->argv[0])); }

Vm(cons_u) {
  ArityCheck(2);
  Have(2);
  xp = puttwo(hp);
  hp += 2;
  A(xp) = fp->argv[0];
  B(xp) = fp->argv[1];
  return ApC(ret, xp); }
