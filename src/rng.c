#include "i.h"

intptr_t liprng(intptr_t in) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (steele_vigna_2021 * in + 1) >> 8; }
