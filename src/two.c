#include "i.h"

static word cp_two(state v, word x, word *p0, word *t0) {
  pair src = (pair) x,
       dst = bump(v, Width(struct two));
  dst->ap = data, dst->typ = &typ_two,
  dst->a = src->a, dst->b = src->b;
  return (word) (src->ap = (vm*) dst); }

static void wk_two(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), p0, t0);
  B(x) = cp(v, B(x), p0, t0); }

static void tx_two(core v, FILE *o, word x) {
  if (!twop(B(x))) putc('\'', o), transmit(v, o, A(x));
  else for (putc('(', o);; putc(' ', o)) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// FIXME can overflow the stack
static bool eq_two(state f, word x, word y) {
  if (!htwop(ptr(y))) return false;
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

struct typ typ_two = {
  .copy = cp_two, .evac = wk_two, .emit = tx_two, .equal = eq_two, };


two ini_two(two w, word a, word b) {
  w->ap = data, w->typ = &typ_two;
  w->a = a, w->b = b;
  return w; }

pair pairof(core f, word a, word b) {
  if (avail(f) < Width(struct two)) {
    bool ok;
    avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
    if (!ok) return 0; }
  two w = (two) f->hp;
  f->hp += Width(struct two);
  return ini_two(w, a, b); }
