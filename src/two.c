#include "la.h"

// pairs and lists
ob pair(la v, ob a, ob b) {
  two w;
  with(a, with(b, w = cells(v, Width(two))));
  return w ? (ob) ini_two(w, a, b) : 0; }

// length of list
size_t llen(ob l) {
  size_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(Width(two));
  xp = (ob) ini_two(hp, xp, *sp++);
  hp += Width(two);
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
  xp = (ob) ini_two(hp, fp->argv[0], fp->argv[1]);
  hp += Width(two);
  return ApC(ret, xp); }

static Vm(do_two) {
  return ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cp_two) {
  two src = (two) x, dst;
  dst = bump(v, Width(two));
  src->disp = (vm*) dst;
  return (ob) ini_two(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static long em_two(la v, FILE *o, ob x) {
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

static size_t hash_two(la v, ob x) {
  return ror(hash(v, A(x)) * hash(v, B(x)), 4 * sizeof(size_t)); }

static bool eq_two(la v, ob x, ob y) {
  return twop(y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }

const struct mtbl mtbl_two = {
  .does = do_two,
  .emit = em_two,
  .evac = cp_two,
  .hash = hash_two,
  .equi = eq_two, };
