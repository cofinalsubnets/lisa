#include "i.h"

intptr_t liprng(intptr_t seed) {
  const word steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (steele_vigna_2021 * seed + 1) >> 8; }

word l_rand(core f) {
  return f->rand = liprng(f->rand); }
