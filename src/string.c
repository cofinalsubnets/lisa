#include "i.h"
string strof(state f, const char *c) {
  size_t len = strlen(c);
  string o = cells(f, Width(struct string) + b2w(len));
  if (o) o->ap = data, o->typ = String, o->len = len,
         memcpy(o->text, c, len);
  return o; }
