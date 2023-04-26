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
mo mo_n(O f, uintptr_t n) {
  mo k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

static NoInline mo thdr(O f, uintptr_t n, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return mo_n(f, n);
  mo k; avec(f, x, k = thdr(f, n + 1, xs));
  if (k) k[n].x = x;
  return k; }

NoInline mo thd(O f, ...) {
  mo k; va_list xs; return
    va_start(xs, f),
    k = thdr(f, 0, xs),
    va_end(xs),
    k; }
