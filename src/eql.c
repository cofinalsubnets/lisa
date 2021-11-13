#include "lips.h"
#include "eql.h"
#include "two.h"

bool eql(obj a, obj b) {
  if (a == b)             return true;
  if (kind(a) != kind(b)) return false;
  switch (kind(a)) {
    case Two:
      // pairs are immutable, so we can deduplicate their insides.
      if (eql(X(a), X(b))) {
        X(b) = X(a);
        if (eql(Y(a), Y(b))) {
          Y(b) = Y(a);
          return true; } }
      return false;
    case Str: {
      str o = S(a), m = S(b);
      if (o->len != m->len) return false;
      return scmp(o->text, m->text) == 0; }
    default: return false; } }
