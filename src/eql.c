#include "lips.h"
#include "eql.h"

bool eql(obj a, obj b) {
 if (a == b)             return true;
 if (kind(a) != kind(b)) return false;
 switch (kind(a)) {
  case Two:
   // pairs are immutable, so we can deduplicate their contents.
   if (!eql(X(a), X(b))) return false;
   X(a) = X(b);
   if (!eql(Y(a), Y(b))) return false;
   Y(a) = Y(b);
   return true;
  case Str: {
   str o = getstr(a), m = getstr(b);
   return o->len == m->len && scmp(o->text, m->text) == 0; }
  default:  return false; } }
