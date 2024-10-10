#include "i.h"


//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
thread mo_ini(void *_, size_t len) {
  struct tag *t = (void*) ((cell) _ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
thread mo_n(core f, size_t n) {
  thread k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

struct tag *ttag(thread k) {
  return k->x ? ttag(k + 1) : (void*) k; }

Vm(trim) {
  thread k = (thread) sp[0];
  ttag(k)->head = k;
  return op(1, (word) k); }

Vm(seek) {
  thread k = (thread) sp[1];
  return op(2, (word) (k + getnum(sp[0]))); }

Vm(peek) {
  thread k = (thread) sp[0];
  return op(1, k[0].x); }

Vm(poke) {
  thread k = (thread) sp[1];
  k->x = sp[0];
  return op(2, (word) k); }

Vm(thda) {
  size_t n = getnum(sp[0]);
  Have(n + Width(struct tag));
  thread k = mo_ini(memset(hp, -1, n * sizeof(word)), n);
  hp += n + Width(struct tag);
  return op(1, (word) k); }
