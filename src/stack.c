#include "i.h"
#include <stdarg.h>

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

status P1(state f, word x) {
  return push1(f, x) ? Ok : Oom; }
status P2(state f, word x, word y) {
  return push2(f, x, y) ? Ok : Oom; }
