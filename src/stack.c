#include "i.h"
#include <stdarg.h>

static NoInline word pushsr(core f, size_t m, size_t n, va_list xs) {
  if (!n) return please(f, m) ? m : n;
  word x = va_arg(xs, word), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

word pushs(core f, size_t m, ...) {
  va_list xs; va_start(xs, m);
  word r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else {
    f->sp -= m;
    for (size_t n = 0; n < m; f->sp[n++] = r = va_arg(xs, word)); }
  va_end(xs);
  return r; }
