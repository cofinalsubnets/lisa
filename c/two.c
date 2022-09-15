#include "la.h"

static ob pair_ok(la, ob, ob) NoInline,
          pair_gc(la, ob, ob) NoInline;

// pairs and lists

ob pair(la v, ob a, ob b) { return
  Avail < 2 ? pair_gc(v, a, b) : pair_ok(v, a, b); }

static NoInline ob pair_ok(la v, ob a, ob b) {
  two w = bump(v, 2);
  return w->a = a, w->b = b, puttwo(w); }

// functions for pairs and lists
static NoInline ob pair_gc(la v, ob a, ob b) {
  bool _;
  with(a, with(b, _ = please(v, 2)));
  return _ ? pair_ok(v, a, b) : 0; }

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
  return
    hp[0] = xp,
    hp[1] = *sp++,
    xp = puttwo(hp),
    hp += 2,
    ApN(1, xp); }

Vm(car_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, A(*fp->argv)); }

Vm(cdr_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Two);
  return ApC(ret, B(fp->argv[0])); }

Vm(cons_u) {
  ArityCheck(2);
  Have(2);
  return
    xp = puttwo(hp),
    hp += 2,
    A(xp) = fp->argv[0],
    B(xp) = fp->argv[1],
    ApC(ret, xp); }
