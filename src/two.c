#include "i.h"

// list length
size_t llen(word l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

// index of item in list
long lidx(state f, word l, word x) {
  for (long i = 0; twop(l); l = B(l), i++)
    if (eql(f, A(l), x)) return i;
  return -1; }

word assoc(state f, word l, word k) {
  for (; twop(l); l = B(l))
    if (eql(f, k, A(A(l)))) return A(l);
  return 0; }

word lookup(state f, word l, word k) {
  return (l = assoc(f, l, k)) ? B(l) : l; }

word dict_lookup(state f, word k) {
  return lookup(f, f->dict, k); }

two ini_two(two w, word a, word b) {
  w->ap = data, w->typ = Pair;
  w->a = a, w->b = b;
  return w; }

pair cons(state f, word a, word b) {
  if (avail(f) < Width(struct two)) {
    bool ok;
    avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
    if (!ok) return 0; }
  return ini_two(bump(f, Width(struct two)), a, b); }
