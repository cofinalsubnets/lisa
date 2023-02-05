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

// try to get the name of a function
ob hnom(li v, mo x) {
  if (!livep(v, (ob) x)) return nil;
  vm *k = G(x);

  if (k == setclo || k == genclo0 || k == genclo1) // closure?
    return hnom(v, (mo) G(FF(x)));

  ob n = ((ob*) mo_tag(x))[-1];
  return homp(n) && livep(v, n) && G(n) == act ? n : nil; }
