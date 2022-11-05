#include "la.h"

// pairs and lists
static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok;
  with(a, with(b, ok = please(v, Width(two))));
  return ok ? pair(v, a, b) : 0; }

two pair(la v, ob a, ob b) {
  return Avail >= Width(two) ?
    ini_two(bump(v, Width(two)), a, b) :
    pair_gc(v, a, b); }

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(Width(two));
  xp = (ob) ini_two(hp, xp, *sp++);
  hp += Width(two);
  return ApN(1, xp); }

Vm(car_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, A(xp)); }

Vm(cdr_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(twop(xp));
  return ApC(ret, B(xp)); }

Vm(cons_f) {
  ArityCheck(2);
  Have(Width(two));
  xp = (ob) ini_two(hp, fp->argv[0], fp->argv[1]);
  hp += Width(two);
  return ApC(ret, xp); }

static Vm(aptwo) {
  return ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cptwo) {
  two src = (two) x,
      dst = bump(v, Width(two));
  src->head.disp = (vm*) dst;
  return (ob) ini_two(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static long txtwo(la v, FILE *o, ob x) {
  long r = 2;
  if (fputc('(', o) == EOF) return -1;
  for (;;) {
    long i = la_tx(v, o, A(x));
    if (i < 0) return i;
    else r += i;
    if (!twop(x = B(x))) break;
    else if (fputc(' ', o) == EOF) return -1;
    else r++; }
  if (fputc(')', o) == EOF) return -1;
  return r; }

static intptr_t hxtwo(la v, ob x) {
  intptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(intptr_t)); }

static bool eqtwo(la v, ob x, ob y) {
  return twop(y) &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

const struct mtbl mtbl_two = {
  .does = aptwo,
  .emit = txtwo,
  .evac = cptwo,
  .hash = hxtwo,
  .equi = eqtwo, };
