#include "i.h"
#include <stdarg.h>

// list length
size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

long lidx(state f, word l, word x) {
  for (long i = 0; twop(l); l = B(l), i++)
    if (eql(f, A(l), x)) return i;
  return -1; }

word lookup(state f, word l, word k) {
  for (; twop(l); l = B(l))
    if (eql(f, k, A(A(l)))) return B(A(l));
  return 0; }

word dict_lookup(state f, word k) {
  return lookup(f, f->dict, k); }

static NoInline pair cons_(state f, word a, word b) {
  pair w = bump(f, Width(struct pair));
  w->ap = data;
  w->typ = Pair;
  w->a = a;
  w->b = b;
  return w; }

static NoInline pair cons_gc(state f, word a, word b) {
  bool _;
  avec(f, a,
    avec(f, b,
      _ = please(f, Width(struct pair))));
  return _ ? cons_(f, a, b) : 0; }

pair cons(state f, word a, word b) { return
  avail(f) >= Width(struct pair) ?
    cons_(f, a, b) :
    cons_gc(f, a, b); }
