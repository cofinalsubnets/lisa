#include "i.h"

Vm(slen) {
  word x = sp[0];
  ip = (thread) sp[1];
  sp[1] = strp(x) ? putnum(((string)x)->len) : nil;
  return ip->ap(f, ip, hp, sp + 1); }

#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))
Vm(ssub) {
  thread r = (thread) sp[3];
  if (!strp(sp[0])) sp[3] = nil;
  else {
    string s = (string) sp[0];
    size_t i = nump(sp[1]) ? getnum(sp[1]) : 0,
           j = nump(sp[2]) ? getnum(sp[2]) : 0;
    i = max(i, 0), j = min(j, s->len);
    Have(Width(struct string) + b2w(j - i));
    string t = ini_str((string) hp, j - i);
    memcpy(t->text, s->text + i, j);
    sp[3] = (word) t; }
  return r->ap(f, r, hp, sp + 3); }

Vm(sget) {
  thread r = (thread) sp[2];
  if (!strp(sp[0])) sp[2] = nil;
  else {
    string s = (string) sp[0];
    size_t i = min(s->len - 1, getnum(sp[1]));
    i = max(i, 0);
    sp[2] = putnum(s->text[i]); }
  return r->ap(f, r, hp, sp + 2); }

string ini_str(string s, size_t len) {
  s->ap = data, s->typ = String, s->len = len;
  return s; }

string strof(state f, const char *c) {
  size_t len = strlen(c);
  string o = cells(f, Width(struct string) + b2w(len));
  if (o) memcpy(ini_str(o, len)->text, c, len);
  return o; }
