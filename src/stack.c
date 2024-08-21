#include "i.h"
#include <stdarg.h>

static NoInline word pushsr(state l, size_t m, size_t n, va_list xs) {
  if (!n) return please(l, m) ? m : n;
  word x = va_arg(xs, word), y;
  avec(l, x, y = pushsr(l, m, n - 1, xs));
  return y ? *--l->sp = x : y; }

word pushs(state l, size_t m, ...) {
  va_list xs; va_start(xs, m);
  word r = 0;
  if (avail(l) < m) r = pushsr(l, m, m, xs);
  else {
    l->sp -= m;
    for (size_t n = 0; n < m; l->sp[n++] = r = va_arg(xs, word)); }
  va_end(xs);
  return r; }

static NoInline thread thdr(state l, size_t m, size_t n, va_list xs) {
  thread r;
  if (!n) return (r = mo_n(l, m)) ? r + m : r;
  word x = va_arg(xs, word);
  avec(l, x, r = thdr(l, m, n - 1, xs));
  if (r) (--r)->x = x;
  return r; }

thread thd(state l, size_t m, ...) {
  va_list xs; va_start(xs, m);
  thread r;
  if (avail(l) < m + Width(struct tag)) r = thdr(l, m, m, xs);
  else for (thread s = r = mo_n(l, m); m--; s++->x = va_arg(xs, word));
  va_end(xs);
  return r; }
