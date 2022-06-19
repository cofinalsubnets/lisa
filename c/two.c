#include "la.h"

// functions for pairs and lists
static ob pair_(la v, ob a, ob b) {
  bool _; two w;
  return with(a, with(b, _ = please(v, 2))), !_ ? 0 :
    (w = bump(v, 2), w->a = a, w->b = b, put2(w)); }

ob pair(la v, ob a, ob b) {
  two w; return Avail < 2 ? pair_(v, a, b) :
    (w = bump(v, 2), w->a = a, w->b = b, put2(w)); }

N llen(ob l) {
  N i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

// pairs
Op(1, car, A(xp))
Op(1, cdr, B(xp))

Vm(cons) {
  Have1();
  return
    hp[0] = xp,
    hp[1] = *sp++,
    xp = put2(hp),
    hp += 2,
    ApN(1, xp); }

Vm(car_u) {
  Ary(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, A(*fp->argv)); }

Vm(cdr_u) {
  Ary(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, B(*fp->argv)); }

Ll(cons_u) {
  Ary(2);
  Have(2);
  two w; return
    w = (two) hp,
    hp += 2,
    w->a = fp->argv[0],
    w->b = fp->argv[1],
    ApC(ret, put2(w)); }
