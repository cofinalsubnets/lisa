#include "i.h"

// pairs and lists
static size_t llenr(ob l, size_t n) {
  return twop(l) ? llenr(B(l), n + 1) : n; }
size_t llen(ob l) { return llenr(l, 0); }

static Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, Width(struct two));
  src->act = (vm*) dst;
  return (ob) two_ini(dst, src->a, src->b); }

static void wk_two(li v, ob x, ob *pool0, ob *top0) {
  B(x) = cp(v, B(x), pool0, top0); 
  A(x) = cp(v, A(x), pool0, top0);
  v->cp += Width(struct two); }

static void tx_two(la v, FILE* o, ob x) {
  putc('(', o);
  for (;;) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) break;
    putc(' ', o); }
  putc(')', o); }

static uintptr_t hx_two(la v, ob x) {
  uintptr_t hc = hash(v, A(x)) * hash(v, B(x));
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
  .walk = wk_two,
  .equi = eq_two, };
