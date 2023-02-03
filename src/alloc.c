#include "i.h"

// push things onto the stack
static NoInline bool pushsr(la v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

NoInline bool pushs(la v, ...) {
  bool _; va_list xs; return
  va_start(xs, v),
  _ = pushsr(v, 0, xs),
  va_end(xs),
  _; }
