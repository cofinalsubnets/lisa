#include "la.h"

// freestanding libc substitutes

intptr_t lcprng(intptr_t s) {
  const int64_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (s * steele_vigna_2021 + 1) >> 8; }

void *setw(void *x, intptr_t i, size_t l) {
  while (l--) ((intptr_t*) x)[l] = i;
  return x; }

void *cpyw(void *x, const void *y, size_t l) {
  for (size_t i = 0; i < l; i++)
    ((void**)x)[i] = ((void**)y)[i];
  return x; }
