#include "i.h"
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.

// allocate a thread
mo mo_n(li v, size_t n) {
  mo k = cells(v, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

static NoInline mo thdr(li v, size_t n, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return mo_n(v, n);
  mo k; with(x, k = thdr(v, n + 1, xs));
  if (k) k[n].x = x;
  return k; }

NoInline mo thd(li v, ...) {
  mo k; va_list xs; return
    va_start(xs, v),
    k = thdr(v, 0, xs),
    va_end(xs),
    k; }
