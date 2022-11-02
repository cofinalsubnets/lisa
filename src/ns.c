#include "la.h"

ob ns_get(la v, ob y) {
  return tbl_get(v, v->topl, y); }

ob ns_set(la v, ob y, ob x) {
  return tbl_set(v, v->topl, y, x); }

ob ns_tbl(la v) { return v->topl; }
