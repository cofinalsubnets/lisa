#include "i.h"

// function functions
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
mo mo_n(la v, U n) {
  mo k = cells(v, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

static NoInline mo thdr(li v, size_t n, va_list xs) {
  vm *x = va_arg(xs, vm*);
  if (!x) return mo_n(v, n);
  mo k; with(x, k = thdr(v, n + 1, xs));
  if (k) k[n].ap = x;
  return k; }

NoInline mo thd(li v, ...) {
  va_list xs;
  va_start(xs, v);
  mo k = thdr(v, 0, xs);
  va_end(xs);
  return k; }

// try to get the name of a function
ob hnom(li v, mo x) {
  if (!livep(v, (ob) x)) return nil;
  vm *k = G(x);

  if (k == setclo || k == genclo0 || k == genclo1) // closure?
    return hnom(v, (mo) G(FF(x)));

  ob n = ((ob*) mo_tag(x))[-1];
  return homp(n) && livep(v, n) && G(n) == act ? n : nil; }

