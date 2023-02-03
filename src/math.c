#include "i.h"

// rng
uintptr_t liprng(li v) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  uintptr_t r = (steele_vigna_2021 * v->rand + 1) >> 8;
  return v->rand = r; }
