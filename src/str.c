#include "i.h"
const struct typ str_typ = {
  .does = do_id, .emit = tx_str, .evac = cp_str,
  .hash = hx_str, .walk = wk_str, .equi = eq_str, };

str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  if (o) memcpy(str_ini(o, bs)->text, c, bs);
  return o; }

bool eq_str(li v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }
