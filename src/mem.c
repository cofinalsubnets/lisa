#include "la.h"

// general memory allocation / access functions

// unchecked allocator -- make sure there's enough memory!
void *bump(la v, size_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

void *cells(la v, size_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

void *setw(void *x, intptr_t i, size_t l) {
  while (l--) ((intptr_t*) x)[l] = i;
  return x; }

void *cpyw(void *restrict x, const void *restrict y, size_t l) {
  for (size_t i = 0; i < l; i++)
    ((void**)x)[i] = ((void**)y)[i];
  return x; }
