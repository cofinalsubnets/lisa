#include "i.h"
#include <stdarg.h>

static NoInline word pushsr(core f, size_t m, size_t n, va_list xs) {
  if (!n) return f->please(f, m) ? m : n;
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

static NoInline thread thdr(core f, size_t m, size_t n, va_list xs) {
  thread r;
  if (!n) return (r = mo_n(f, m)) ? r + m : r;
  word x = va_arg(xs, word);
  avec(f, x, r = thdr(f, m, n - 1, xs));
  if (r) (--r)->x = x;
  return r; }

thread thd(core f, size_t m, ...) {
  va_list xs; va_start(xs, m);
  thread r;
  if (avail(f) < m + Width(struct tag)) r = thdr(f, m, m, xs);
  else for (thread s = r = mo_n(f, m); m--; s++->x = va_arg(xs, word));
  va_end(xs);
  return r; }
