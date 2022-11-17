#include "la.h"

NoInline ob ns_seek(la v, tbl ns, ob n) {
  ob x = tbl_get(v, ns, n, 0);
  if (x) return x;
  x = tbl_get(v, ns, nil, 0);
  if (!x) return x;
  while (twop(x))
    if (lidx(BA(x), n) != -1)
      return tbl_get(v, (tbl) AA(x), n, 0);
  return 0; }

ob nsget(la v, ob y) {
  return tbl_get(v, v->topl, y, 0); }

bool nsset(la v, ob y, ob x) {
  return tbl_set(v, v->topl, y, x); }

ob nstbl(la v) { return (ob) v->topl; }
