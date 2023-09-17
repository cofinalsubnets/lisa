#include "i.h"
#include <stdarg.h>

static two pair_gc(state, word, word);
static word push1_gc(state, word),
            push2_gc(state, word, word);

word push1(state l, word x) { return
  avail(l) ? *--l->sp = x : push1_gc(l, x); }

word push2(state l, word x, word y) {
  if (avail(l) < 2) return push2_gc(l, x, y);
  word *sp = l->sp -= 2;
  return sp[1] = y, sp[0] = x; }

two pair(state f, word a, word b) {
  return avail(f) < Width(struct two) ?
    pair_gc(f, a, b) :
    two_ini(bump(f, Width(struct two)), a, b); }

str strof(state f, const char* c) {
  size_t bs = strlen(c);
  str o = cells(f, Width(struct str) + b2w(bs));
  if (o) str_ini(o, bs),
         memcpy(o->text, c, bs);
  return o; }

str str_ini(void *_, size_t len) {
  str s = _; return
    s->ap = data,
    s->mtd = &str_methods,
    s->len = len,
    s; }

struct tag *mo_tag(verb k) {
  return k->x ? mo_tag(k + 1) : (void*) k; }

two two_ini(void *_, word a, word b) {
  two w = _; return
    w->ap = data, w->mtd = &two_methods,
    w->_[0] = a, w->_[1] = b, w; }

#define End ((intptr_t)0)
static NoInline word listr(state f, va_list xs) {
  word y, x = va_arg(xs, word);
  if (!x) return nil;
  avec(f, x, y = listr(f, xs));
  return y ? (word) pair(f, x, y) : y; }

NoInline word list(state f, ...) {
  word x; va_list xs;
  va_start(xs, f), x = listr(f, xs), va_end(xs);
  return x; }

static NoInline word push1_gc(state l, word x) {
  bool ok; avec(l, x, ok = please(l, 1));
  return ok ? push1(l, x) : 0; }

static NoInline word push2_gc(state l, word x, word y) {
  bool ok; avec(l, x, avec(l, y, ok = please(l, 2)));
  return ok ? push2(l, x, y) : 0; }
static NoInline two pair_gc(state f, word a, word b) {
  bool ok; avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
  return ok ? pair(f, a, b) : 0; }
