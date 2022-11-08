#include "la.h"

// general memory allocation / access functions

// unchecked allocator -- make sure there's enough memory!
void *bump(la v, size_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

void *cells(la v, size_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }
