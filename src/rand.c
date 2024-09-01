#include "i.h"

word liprng(core v) {
  const word steele_vigna_2021 = 0xaf251af3b0f025b5;
  return v->rand = (steele_vigna_2021 * v->rand + 1) >> 8; }
