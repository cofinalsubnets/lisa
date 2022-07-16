#include "la.h"

ob refer(pt v, ob _) {
  ob x, mod = v->wns;
  for (; twop(mod); mod = B(mod))
    if ((x = tbl_get(v, A(mod), _))) return x;
  return 0; }

Ll(cwm_u) { return ApC(ret, v->wns); }
Ll(popd_u) {
  xp = B(v->wns);
  if (twop(xp)) v->wns = xp;
  return ApC(ret, nil); }
