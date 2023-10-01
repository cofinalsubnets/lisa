#include "i.h"
#include <stdarg.h>

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
thread mo_ini(void *_, size_t len) {
  struct loop *t = (void*) ((cell) _ + len);
  return t->null = NULL, t->head = _; }

// allocate a thread
thread mo_n(state f, size_t n) {
  thread k = cells(f, n + Width(struct loop));
  return !k ? k : mo_ini(k, n); }

struct loop *mo_tag(thread k) {
  return k->x ? mo_tag(k + 1) : (void*) k; }
