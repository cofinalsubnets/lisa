#include "i.h"
// push things onto the stack
static NoInline bool pushsr(la v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

bool pushs(la v, ...) {
  bool _; va_list xs; return
  va_start(xs, v),
  _ = pushsr(v, 0, xs),
  va_end(xs),
  _; }

// unchecked allocator -- make sure there's enough memory!
void *bump(la v, size_t n) {
  void *x = v->hp;
  return v->hp += n, x; }

void *cells(la v, size_t n) { return
  Avail >= n || please(v, n) ? bump(v, n) : 0; }
