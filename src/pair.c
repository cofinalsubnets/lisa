#include "i.h"
#include <stdarg.h>

static NoInline pair cons_gc(state f, word a, word b) {
  pair w;
  avec(f, a, avec(f, b, w = cells(f, Width(struct pair))));
  if (w) w->ap = data, w->typ = Pair, w->a = a, w->b = b;
  return w; }

pair cons(state f, word a, word b) {
  if (avail(f) < Width(struct pair)) return cons_gc(f, a, b);
  pair w = bump(f, Width(struct pair));
  w->ap = data, w->typ = Pair, w->a = a, w->b = b;
  return w; }

// list length
size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

long lidx(state f, word l, word x) {
  for (long i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }
