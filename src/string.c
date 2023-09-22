#include "i.h"
#include <stdarg.h>

str strof(state f, const char* c) {
  size_t bs = strlen(c);
  str o = cells(f, Width(struct string) + b2w(bs));
  if (o) str_ini(o, bs),
         memcpy(o->text, c, bs);
  return o; }

str str_ini(void *_, size_t len) {
  str s = _; return
    s->ap = data,
    s->typ = String,
    s->len = len,
    s; }
