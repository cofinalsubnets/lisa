#include "la.h"

two ini_two(void *_, ob a, ob b) {
  two w = _;
  w->h.disp = disp, w->h.mtbl = &mtbl_two;
  w->a = a, w->b = b;
  return w; }

// pairs and lists
static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok;
  with(a, with(b, ok = please(v, wsizeof(struct two))));
  return ok ? pair(v, a, b) : 0; }

two pair(la v, ob a, ob b) {
  return Avail >= wsizeof(struct two) ?
    ini_two(bump(v, wsizeof(struct two)), a, b) :
    pair_gc(v, a, b); }

// length of list
size_t llen(ob l) {
  size_t i = 0;
  while (twop(l)) l = B(l), i++;
  return i; }

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(wsizeof(struct two));
  xp = (ob) ini_two(hp, xp, *sp++);
  hp += wsizeof(struct two);
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
  if (fp->argc) {
    size_t n = wsizeof(struct two) * (fp->argc - 1);
    Have(n);
    two w = (two) hp;
    hp += n;
    xp = fp->argv[fp->argc-1];
    for (size_t i = fp->argc - 1; i--;
      xp = (ob) ini_two(w+i, fp->argv[i], xp)); }
  return ApC(ret, xp); }

static Vm(ap_two) { return
  ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, wsizeof(struct two));
  src->h.disp = (vm*) dst;
  return (ob) ini_two(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static void tx_two(la v, la_io o, ob x) {
  la_putc('(', o);
  for (;;) {
    la_tx(v, o, A(x));
    if (!twop(x = B(x))) break;
    la_putc(' ', o); }
  la_putc(')', o); }

static intptr_t hx_two(la v, ob x) {
  intptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(intptr_t)); }

static bool eq_two(la v, ob x, ob y) {
  return twop(y) &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

const struct mtbl mtbl_two = {
  .does = ap_two,
  .emit = tx_two,
  .evac = cp_two,
  .hash = hx_two,
  .equi = eq_two, };
