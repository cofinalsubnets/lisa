#include "la.h"

// functions for pairs and lists
static ob pair_(la v, ob a, ob b) {
  bool _;
  with(a, with(b, _ = please(v, 2)));
  return _ ? pair(v, a, b) : 0; }

ob pair(la v, ob a, ob b) {
  if (Avail < 2) return pair_(v, a, b);
  two w = bump(v, 2);
  w->a = a;
  w->b = b;
  return put2(w); }

uintptr_t llen(ob l) {
  uintptr_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

// pairs
Op(1, car, A(xp))
Op(1, cdr, B(xp))

Vm(cons) {
  if (Slack == 0) return Pray(1);
  hp[0] = xp;
  hp[1] = *sp++;
  xp = put2(hp);
  hp += 2;
  return ApN(1, xp); }

Vm(car_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, A(*fp->argv)); }

Vm(cdr_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, B(*fp->argv)); }

Ll(cons_u) {
  ArityCheck(2);
  if (Slack < 2) return Pray(2);
  two w = (two) hp;
  hp += 2;
  w->a = fp->argv[0];
  w->b = fp->argv[1];
  return ApC(ret, put2(w)); }
