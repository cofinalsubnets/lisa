#include "i.h"

static NoInline word push1_gc(state l, word x) {
  bool ok; avec(l, x, ok = please(l, 1));
  return ok ? push1(l, x) : 0; }

word push1(state l, word x) { return
  avail(l) ? *--l->sp = x : push1_gc(l, x); }

static NoInline word push2_gc(state l, word x, word y) {
  bool ok; avec(l, x, avec(l, y, ok = please(l, 2)));
  return ok ? push2(l, x, y) : 0; }

word push2(state l, word x, word y) {
  if (avail(l) < 2) return push2_gc(l, x, y);
  word *sp = l->sp -= 2;
  return sp[1] = y, sp[0] = x; }

static NoInline word push3_gc(state f, word x, word y, word z) {
  bool ok;
  avec(f, x, avec(f, y, avec(f, z, ok = please(f, 3))));
  return ok ? push3(f, x, y, z) : 0; }

word push3(state f, word x, word y, word z) {
  if (avail(f) < 3) return push3_gc(f, x, y, z);
  word *sp = f->sp -= 3;
  return sp[2] = z, sp[1] = y, sp[0] = x; }

