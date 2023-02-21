#include "i.h"

NoInline two pair(li v, ob a, ob b) {
  if (Avail < Width(struct two)) {
    bool ok; with(a, with(b, ok = please(v, Width(struct two))));
    if (!ok) return NULL; }
  return two_ini(bump(v, Width(struct two)), a, b); }

  // pairs
Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(Width(struct two));
  xp = (ob) two_ini(hp, xp, *sp++);
  hp += Width(struct two);
  return ApN(1, xp); }

Vm(car_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? A(xp) : xp;
  return ApC(ret, xp); }

Vm(cdr_f) {
  if (fp->argc)
    xp = fp->argv[0],
    xp = twop(xp) ? B(xp) : nil;
  return ApC(ret, xp); }

Vm(cons_f) {
  if (fp->argc) {
    size_t n = Width(struct two) * (fp->argc - 1);
    Have(n);
    two w = (two) hp;
    hp += n;
    xp = fp->argv[fp->argc-1];
    for (size_t i = fp->argc - 1; i--;
      xp = (ob) two_ini(w+i, fp->argv[i], xp)); }
  return ApC(ret, xp); }
