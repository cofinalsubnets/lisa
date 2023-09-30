#include "i.h"

typedef bool equator(state, word, word);
static equator eq_str, eq_two;
static equator *eqs[] = { [Pair] = eq_two, [String] = eq_str, };

bool eql(state f, word a, word b) {
  if (a == b) return true;
  if (nump(a | b) || ptr(a)->ap != data) return false;
  return eqs[ptr(a)[1].x](f, a, b); }

// FIXME can overflow the stack
static bool eq_two(state f, word x, word y) {
  if (!htwop(ptr(y))) return false;
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static bool eq_str(state f, word x, word y) {
  if (!hstrp((thread) y)) return false;
  string a = (string) x, b = (string) y;
  if (a->len != b->len) return false;
  return 0 == strncmp(a->text, b->text, a->len); }
