#include "i.h"

typedef bool equator(state, word, word);
static equator eq_str, eq_two;
static equator *eqs[] = { [Pair] = eq_two, [String] = eq_str, };

bool eql(state v, word a, word b) { return a == b ||
  (!nump(a|b) && datp((verb) a) && eqs[((verb) a)[1].x](v, a, b)); }

// FIXME can overflow the stack
static bool eq_two(state v, word x, word y) { return
  htwop((verb) y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }

static bool eq_str(state v, word x, word y) {
  if (!hstrp((verb) y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }
