#include "i.h"

bool eql(state v, word a, word b) { return a == b ||
  (!nump(a|b) && datp((verb) a) && mtd((verb) a)->equi(v, a, b)); }

// FIXME can overflow the stack
bool eq_two(state v, word x, word y) { return
  htwop((verb) y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }

bool eq_str(state v, word x, word y) {
  if (!hstrp((verb) y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }
