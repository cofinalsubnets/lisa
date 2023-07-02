#include "i.h"

static two pair_gc(state, ob, ob);
static ob push1_gc(state, ob),
          push2_gc(state, ob, ob);

ob push1(state l, ob x) { return
  avail(l) ? *--l->sp = x : push1_gc(l, x); }

ob push2(state l, ob x, ob y) {
  if (avail(l) < 2) return push2_gc(l, x, y);
  word *sp = l->sp -= 2;
  return sp[1] = y, sp[0] = x; }

two pair(state f, ob a, ob b) { return
  avail(f) < Width(struct two) ? pair_gc(f, a, b) :
    two_ini(bump(f, Width(struct two)), a, b); }

str strof(state f, const char* c) {
  size_t bs = strlen(c);
  str o = cells(f, Width(struct str) + b2w(bs));
  if (o) memcpy(str_ini(o, bs)->text, c, bs);
  return o; }

str str_ini(void *_, size_t len) {
  str s = _; return
    s->ap = data, s->mtd = &str_methods,
    s->len = len, s; }

two two_ini(void *_, word a, word b) {
  two w = _; return
    w->ap = data, w->mtd = &two_methods,
    w->_[0] = a, w->_[1] = b, w; }

static NoInline ob push1_gc(state l, ob x) {
  bool ok; avec(l, x, ok = please(l, 1));
  return ok ? push1(l, x) : 0; }
static NoInline ob push2_gc(state l, ob x, ob y) {
  bool ok; avec(l, x, avec(l, y, ok = please(l, 2)));
  return ok ? push2(l, x, y) : 0; }
static NoInline two pair_gc(state f, word a, word b) {
  bool ok; avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
  return ok ? pair(f, a, b) : 0; }
