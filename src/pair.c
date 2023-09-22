#include "i.h"
#include <stdarg.h>

static NoInline two pair_gc(state f, word a, word b) {
  bool ok; avec(f, a, avec(f, b, ok = please(f, Width(struct pair))));
  return ok ? cons(f, a, b) : 0; }

two cons(state f, word a, word b) {
  return avail(f) < Width(struct pair) ?
    pair_gc(f, a, b) :
    two_ini(bump(f, Width(struct pair)), a, b); }

two two_ini(void *_, word a, word b) {
  two w = _; return
    w->ap = data, w->typ = Pair,
    w->_[0] = a, w->_[1] = b, w; }

#define End ((intptr_t)0)
static NoInline word listr(state f, va_list xs) {
  word y, x = va_arg(xs, word);
  if (!x) return nil;
  avec(f, x, y = listr(f, xs));
  return y ? (word) cons(f, x, y) : y; }

NoInline word list(state f, ...) {
  word x; va_list xs;
  va_start(xs, f), x = listr(f, xs), va_end(xs);
  return x; }

// list length
size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

long lidx(state f, word l, word x) {
  for (long i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }
