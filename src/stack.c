#include "i.h"
#include <stdarg.h>

static word push1_gc(state, word),
            push2_gc(state, word, word);

word push1(state l, word x) { return
  avail(l) ? *--l->sp = x : push1_gc(l, x); }

word push2(state l, word x, word y) {
  if (avail(l) < 2) return push2_gc(l, x, y);
  word *sp = l->sp -= 2;
  return sp[1] = y, sp[0] = x; }

static NoInline word push1_gc(state l, word x) {
  bool ok; avec(l, x, ok = please(l, 1));
  return ok ? push1(l, x) : 0; }

static NoInline word push2_gc(state l, word x, word y) {
  bool ok; avec(l, x, avec(l, y, ok = please(l, 2)));
  return ok ? push2(l, x, y) : 0; }
