#include "lips.h"
#include "two.h"

u64 llen(obj l) {
  for (u64 i = 0;; l = B(l), i++)
    if (!twop(l)) return i; }

#include "mem.h"
// functions for pairs and lists
obj pair(lips v, obj a, obj b) {
  two w;
  with(a, with(b, w = cells(v, 2)));
  bind(w, w);
  w->a = a, w->b = b;
  return puttwo(w); }

#include "terp.h"
#include "hom.h"
// pairs
OP1(car, A(xp)) OP1(cdr, B(xp))
Vm(cons) {
  Have1();
  hp[0] = xp;
  hp[1] = *sp++;
  xp = puttwo(hp);
  hp += 2;
  Next(1); }

Vm(car_u) {
  Ary(1);
  Tc(*Argv, Two);
  Go(ret, A(*Argv)); }

Vm(cdr_u) {
  Ary(1);
  Tc(*Argv, Two);
  Go(ret, B(*Argv)); }

Vm(cons_u) {
  Ary(2);
  Have(2);
  two w = (two) hp;
  hp += 2;
  w->a = Argv[0], w->b = Argv[1];
  Go(ret, puttwo(w)); }
