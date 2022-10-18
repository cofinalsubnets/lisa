#include "lisa.h"
#include "vm.h"

// pairs and lists
ob pair(la v, ob a, ob b) {
  two w;
  with(a, with(b, w = cells(v, 2)));
  if (!w) return 0;
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
  TypeCheck(Argv[0], Two);
  return ApC(ret, A(Argv[0])); }

Vm(cdr_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Two);
  return ApC(ret, B(Argv[0])); }

Vm(cons_u) {
  ArityCheck(2);
  Have(2);
  hp[0] = Argv[0];
  hp[1] = Argv[1];
  xp = puttwo(hp);
  hp += 2;
  return ApC(ret, xp); }
