#include "i.h"

static NoInline word push1_gc(state f, word x) {
  bool ok; avec(f, x, ok = please(f, 1));
  return ok ? (*--f->sp = x) : 0; }

word push1(state f, word x) {
  return avail(f) ? (*--f->sp = x) : push1_gc(f, x); }

// push things onto the stack
static NoInline bool pushsr(state f, size i, va_list xs) {
  bool _; word x = va_arg(xs, word);
  if (!x) return avail(f) >= i || please(f, i);
  avec(f, x, _ = pushsr(f, i + 1, xs));
  if (!_) return _;
  *--f->sp = x;
  return true; }

NoInline bool pushs(state f, ...) {
  bool _; va_list xs;
  va_start(xs, f), _ = pushsr(f, 0, xs), va_end(xs);
  return _; }

static NoInline word listr(state f, va_list xs) {
  word y, x = va_arg(xs, word);
  if (!x) return nil;
  avec(f, x, y = listr(f, xs));
  return y ? (word) pair(f, x, y) : y; }

NoInline word list(state f, ...) {
  word x; va_list xs;
  va_start(xs, f), x = listr(f, xs), va_end(xs);
  return x; }
