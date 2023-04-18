#include "i.h"

NoInline two pair(li v, ob a, ob b) {
  if (Avail < Width(struct two)) {
    bool ok; with(a, with(b, ok = please(v, Width(struct two))));
    if (!ok) return NULL; }
  return two_ini(bump(v, Width(struct two)), a, b); }
