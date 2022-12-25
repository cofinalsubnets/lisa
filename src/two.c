#include "la.h"

two two_ini(void *_, ob a, ob b) {
  two w = _;
  w->h.disp = disp, w->h.mtbl = &mtbl_two;
  w->a = a, w->b = b;
  return w; }

// pairs and lists
static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok;
  with(a, with(b, ok = please(v, wsizeof(struct two))));
  return ok ? pair(v, a, b) : 0; }

NoInline two pair(la v, ob a, ob b) {
  return Avail >= wsizeof(struct two) ?
    two_ini(bump(v, wsizeof(struct two)), a, b) :
    pair_gc(v, a, b); }

// length of list
size_t llen(ob l) {
  for (size_t i = 0;;)
    if (twop(l)) l = B(l), i++;
    else return i; }

Vm(car) { return ApN(1, A(xp)); }
Vm(cdr) { return ApN(1, B(xp)); }

Vm(cons) {
  Have(wsizeof(struct two));
  xp = (ob) two_ini(hp, xp, *sp++);
  hp += wsizeof(struct two);
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
    size_t n = wsizeof(struct two) * (fp->argc - 1);
    Have(n);
    two w = (two) hp;
    hp += n;
    xp = fp->argv[fp->argc-1];
    for (size_t i = fp->argc - 1; i--;
      xp = (ob) two_ini(w+i, fp->argv[i], xp)); }
  return ApC(ret, xp); }

static Vm(ap_two) { return
  ApC(ret, fp->argc ? B(ip) : A(ip)); }

static Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, wsizeof(struct two));
  src->h.disp = (vm*) dst;
  return (ob) two_ini(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static void tx_two(la v, la_io o, ob x) {
  putc('(', o);
  for (;;) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) break;
    putc(' ', o); }
  putc(')', o); }

static I hx_two(la v, ob x) {
  I hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(I)); }

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
