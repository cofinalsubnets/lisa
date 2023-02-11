#include "i.h"

const struct typ
  two_typ = { .does = do_two, .emit = tx_two, .evac = cp_two,
              .hash = hx_two, .walk = wk_two, .equi = eq_two, };

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

// pairs and lists
static size_t llenr(ob l, size_t n) {
  return twop(l) ? llenr(B(l), n + 1) : n; }
size_t llen(ob l) { return llenr(l, 0); }

bool eq_two(la v, ob x, ob y) {
  return gettyp(y) == &two_typ &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

Vm(do_two) { return ApC(ret, fp->argc ? B(ip) : A(ip)); }
