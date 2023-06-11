#include "i.h"

struct methods
  two_methods = {
    .evac = cp_two,
    .walk = wk_two,
    .emit = tx_two,
    .equi = eq_two,
  },
  str_methods = {
    .evac = cp_str,
    .walk = wk_str,
    .emit = tx_str,
    .equi = eq_str,
  };

bool eql(state v, word a, word b) { return a == b ||
  (!nump(a|b) && datp((verb) a) &&
   gettyp((verb) a)->equi(v, a, b)); }

bool eq_two(state v, word x, word y) { // FIXME can overflow stack
  return htwop((verb) y) &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

bool eq_str(state v, word x, word y) {
  if (!hstrp((verb) y)) return false;
      str a = (str) x, b = (str) y;
      return a->len == b->len && !strncmp(a->text, b->text, a->len); }

intptr_t liprng(intptr_t in) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (steele_vigna_2021 * in + 1) >> 8; }

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
