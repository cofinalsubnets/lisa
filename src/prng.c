#include "i.h"
intptr_t liprng(core v) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return v->rand = (steele_vigna_2021 * v->rand + 1) >> 8; }
