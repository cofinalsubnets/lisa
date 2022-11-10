#include "la.h"
#include "tbl.h"

ob nsget(la v, ob y) {
  return tbl_get(v, v->topl, y, 0); }

bool nsset(la v, ob y, ob x) {
  return tbl_set(v, v->topl, y, x); }

ob nstbl(la v) { return (ob) v->topl; }
