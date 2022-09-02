#include "la.h"

static ob pair_ok(la, ob, ob) NoInline,
          pair_gc(la, ob, ob) NoInline;

// pairs and lists

// cons
ob pair(la v, ob a, ob b) {
  return (Avail < 2 ? pair_gc : pair_ok)(v, a, b); }

static NoInline ob pair_ok(la v, ob a, ob b) {
  two w = bump(v, 2);
  return w->a = a, w->b = b, put2(w); }

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

// index of item in list (-1 if absent)
intptr_t lidx(ob l, ob x) {
  for (intptr_t i = 0; twop(l); l = B(l), i++)
    if (x == A(l)) return i;
  return -1; }

// vm functions
#include "vm.h"

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) { return
  Free == 0 ? Collect(1) :
  (hp[0] = xp,
   hp[1] = *sp++,
   xp = put2(hp),
   hp += 2,
   ApN(1, xp)); }

Vm(car_u) { return
  Arity < 1 ? ArityError(1) :
  TypeOf(fp->argv[0]) != Two ? DomainError() :
  ApC(ret, A(*fp->argv)); }

Vm(cdr_u) { return
  Arity < 1 ? ArityError(1) :
  TypeOf(fp->argv[0]) != Two ? DomainError() :
  ApC(ret, B(fp->argv[0])); }

Vm(cons_u) {
  return
    Free < 2 ? Collect(2) :
    Arity < 2 ? ArityError(2) :
    (xp = put2(hp),
     hp += 2,
     A(xp) = fp->argv[0],
     B(xp) = fp->argv[1],
     ApC(ret, xp)); }
