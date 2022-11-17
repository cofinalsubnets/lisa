#include "la.h"
#include <stdarg.h>

void *cells(la v, size_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

void *cpyw_r2l(void *dst, const void *src, size_t n) {
  while (n--) ((intptr_t*)dst)[n] = ((intptr_t*)src)[n];
  return dst; }

void *cpyw_l2r(void *dst, const void *src, size_t n) {
  for (size_t i = 0; i < n; i++)
    ((intptr_t*)dst)[i] = ((intptr_t*)src)[i];
  return dst; }

void *setw(void *dst, intptr_t w, size_t n) {
  while (n--) ((intptr_t*)dst)[n] = w;
  return dst; }

// unchecked allocator -- make sure there's enough memory!
void *bump(la v, size_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

// general memory allocation / access functions
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
