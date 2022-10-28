#include "lisa.h"
#include "vm.h"

struct mtbl s_mtbl_two = { do_two, em_two, cp_two, hash_two, };

// pairs and lists
ob pair(la v, ob a, ob b) {
  two w;
  with(a, with(b, w = cells(v, Width(two))));
  if (!w) return 0;
  w->disp = disp;
  w->mtbl = mtbl_two;
  w->a = a;
  w->b = b;
  return (ob) w; }

// length of list
size_t llen(ob l) {
  size_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(Width(two));
  two w = (two) hp;
  hp += Width(two);
  w->disp = disp;
  w->mtbl = mtbl_two;
  w->a = xp;
  w->b = *sp++;
  xp = (ob) w;
  return ApN(1, xp); }

Vm(car_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, A(xp)); }

Vm(cdr_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, B(xp)); }

Vm(cons_u) {
  ArityCheck(2);
  Have(Width(two));
  two w = (two) hp;
  hp += Width(two);
  w->disp = disp;
  w->mtbl = mtbl_two;
  w->a = fp->argv[0];
  w->b = fp->argv[1];
  return ApC(ret, (ob) w); }

Vm(do_two) {
  xp = fp->argc == putnum(0) ? A(ip) : B(ip);
  return ApC(ret, xp); }

Gc(cp_two) {
  two src = (two) x, dst;
  dst = bump(v, Width(two));
  src->disp = (vm*) dst;
  dst->disp = disp;
  dst->mtbl = mtbl_two;
  dst->b = cp(v, src->b, pool0, top0);
  dst->a = cp(v, src->a, pool0, top0);
  return (ob) dst; }

void em_two(la v, FILE *o, ob x) {
  for (fputc('(', o);; fputc(' ', o)) {
    tx(v, o, A(x));
    if (!twop(x = B(x))) break; }
  fputc(')', o); }

size_t hash_two(la v, ob x) {
  return ror(hash(v, A(x)) & hash(v, B(x)), 32); }

bool twop(ob _) {
  return homp(_) && GF(_) == (vm*) mtbl_two; }
