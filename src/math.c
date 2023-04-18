#include "i.h"

static bool eq_two(li v, ob x, ob y) { // FIXME can overflow
  return htwop((mo) y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }
static bool eq_str(li v, ob x, ob y) {
  if (!hstrp((mo) y)) return false;
      str a = (str) x, b = (str) y;
      return a->len == b->len && !strncmp(a->text, b->text, a->len); }
static bool (*const data_equi[])(li, ob, ob) = {
 [Two] = eq_two, [Str] = eq_str, };
bool eql(li v, ob a, ob b) { return a == b ||
  (!nump(a|b) && datp((mo) a) &&
   data_equi[gettyp(a)](v, a, b)); }

// rng
intptr_t liprng(intptr_t in) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (steele_vigna_2021 * in + 1) >> 8; }
