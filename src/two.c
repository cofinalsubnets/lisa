#include "i.h"

NoInline two pair(li v, ob a, ob b) {
  if (Avail < Width(struct two)) {
    bool ok; with(a, with(b, ok = please(v, Width(struct two))));
    if (!ok) return NULL; }
  return two_ini(bump(v, Width(struct two)), a, b); }

const struct typ two_typ = {
  .does = do_two, .emit = tx_two, .evac = cp_two,
  .hash = hx_two, .walk = wk_two, .equi = eq_two, };

// pairs and lists
static size_t llenr(ob l, size_t n) {
  return twop(l) ? llenr(B(l), n + 1) : n; }
size_t llen(ob l) { return llenr(l, 0); }

bool eq_two(la v, ob x, ob y) {
  return gettyp(y) == &two_typ &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

Vm(do_two) { return ApC(ret, fp->argc ? B(ip) : A(ip)); }

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
