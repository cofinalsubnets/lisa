#include "la.h"
#include "tbl.h"

ob nsget(la v, ob y) {
  return tblget(v, v->topl, y, 0); }

bool nsset(la v, ob y, ob x) {
  return tblset(v, v->topl, y, x); }

ob nstbl(la v) { return (ob) v->topl; }
