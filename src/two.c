#include "i.h"

// pairs and lists
two two_ini(void *_, ob a, ob b) {
  two w = _; return
    w->act = act,
    w->typ = &two_typ,
    w->a = a, w->b = b,
    w; }

static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok; return
    with(a, with(b, ok = please(v, Width(struct two)))),
    ok ? pair(v, a, b) : 0; }

NoInline two pair(la v, ob a, ob b) {
  return Avail >= Width(struct two) ?
    two_ini(bump(v, Width(struct two)), a, b) :
    pair_gc(v, a, b); }

size_t llenr(ob l, size_t n) {
  return twop(l) ? llenr(B(l), n + 1) : n; }
size_t llen(ob l) { return llenr(l, 0); }
Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, Width(struct two));
  src->act = (vm*) dst;
  return (ob) two_ini(dst,
    cp(v, src->a, pool0, top0),
    cp(v, src->b, pool0, top0)); }

static void tx_two(la v, FILE* o, ob x) {
  putc('(', o);
  for (;;) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) break;
    putc(' ', o); }
  putc(')', o); }

static intptr_t hx_two(la v, ob x) {
  intptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(I)); }

static bool eq_two(la v, ob x, ob y) {
  return (typ) GF(y) == &two_typ &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

static Vm(ap_two) { return
  ApC(ret, fp->argc ? B(ip) : A(ip)); }

const struct typ two_typ = {
  .actn = ap_two,
  .emit = tx_two,
  .evac = cp_two,
  .hash = hx_two,
  .equi = eq_two, };

// Run a GC cycle from inside the VM

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
