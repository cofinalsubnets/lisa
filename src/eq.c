#include "i.h"

bool eql(li v, ob a, ob b) { return a == b ||
  (!nump(a|b) && datp((mo) a) &&
   gettyp((mo) a)->equi(v, a, b)); }

bool eq_two(li v, ob x, ob y) { // FIXME can overflow stack
  return htwop((mo) y) &&
    eql(v, A(x), A(y)) &&
    eql(v, B(x), B(y)); }

bool eq_str(li v, ob x, ob y) {
  if (!hstrp((mo) y)) return false;
      str a = (str) x, b = (str) y;
      return a->len == b->len && !strncmp(a->text, b->text, a->len); }
