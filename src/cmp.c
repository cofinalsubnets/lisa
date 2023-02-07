#include "i.h"

bool neql(la v, ob x, ob y) { return false; }
bool _eql(la v, ob a, ob b) { return
  (!nump(a|b) && G(a) == act &&
   gettyp(a)->equi(v, a, b)); }
