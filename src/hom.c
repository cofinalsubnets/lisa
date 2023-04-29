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
verb mo_n(state f, size n) {
  verb k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

static NoInline verb thdr(state f, size n, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return mo_n(f, n);
  verb k; avec(f, x, k = thdr(f, n + 1, xs));
  if (k) k[n].x = x;
  return k; }

NoInline verb thd(state f, ...) {
  verb k; va_list xs; return
    va_start(xs, f),
    k = thdr(f, 0, xs),
    va_end(xs),
    k; }
