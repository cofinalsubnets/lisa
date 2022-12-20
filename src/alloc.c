#include "la.h"
#include <stdarg.h>
// push things onto the stack
static NoInline bool pushsr(la v, size_t i, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return Avail >= i || please(v, i);
  bool _;
  with(x, _ = pushsr(v, i+1, xs));
  return _ && (*--v->sp = x, true); }

bool pushs(la v, ...) {
  va_list xs;
  va_start(xs, v);
  bool _ = pushsr(v, 0, xs);
  va_end(xs);
  return _; }
