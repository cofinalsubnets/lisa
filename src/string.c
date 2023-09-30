#include "i.h"
string strof(state f, const char *c) {
  size_t len = strlen(c);
  string o = cells(f, Width(struct string) + b2w(len));
  if (o) o->ap = data, o->typ = String, o->len = len,
         memcpy(o->text, c, len);
  return o; }

string buf_new(state f) {
  string s = cells(f, Width(struct string) + 1);
  if (s) s->ap = data, s->typ = String, s->len = sizeof(word);
  return s; }

NoInline string buf_grow(state f, string s) {
  string t; size_t len = s->len;
  avec(f, s, t = cells(f, Width(struct string) + 2 * b2w(len)));
  if (t) t->ap = data, t->typ = String, t->len = 2 * len,
         memcpy(t->text, s->text, len);
  return t; }
