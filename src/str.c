#include "lisa.h"
#include "vm.h"
#include <string.h>

struct mtbl s_mtbl_str = { do_str, em_str, cp_str, hash_str };


ob string(la v, const char* c) {
  intptr_t bs = 1 + slen(c);
  str o = cells(v, Width(str) + b2w(bs));
  if (!o) return 0;
  o->len = bs;
  o->disp = disp;
  o->mtbl = mtbl_str;
  cpy8(o->text, c, bs);
  return putstr(o); }

// string instructions
Vm(slen_u) {
  ArityCheck(1);
  TypeCheck(Argv[0], Str);
  return ApC(ret, putnum(getstr(Argv[0])->len-1)); }

Vm(sget_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Str);
  TypeCheck(Argv[1], Num);
  return ApC(ret,
    getnum(Argv[1]) < getstr(Argv[0])->len-1 ?
      putnum(getstr(Argv[0])->text[getnum(Argv[1])]) :
      nil); }

Vm(scat_u) {
  size_t sum = 0, i = 0, l = getnum(Argc);
  while (i < l) {
    ob x = Argv[i++];
    TypeCheck(x, Str);
    sum += getstr(x)->len - 1; }
  size_t words = Width(str) + b2w(sum+1);
  Have(words);
  str d = (str) hp;
  hp += words;
  d->len = sum + 1;
  d->disp = disp;
  d->mtbl = mtbl_str;
  d->text[sum] = 0;
  while (i) {
    str x = getstr(Argv[--i]);
    sum -= x->len - 1;
    cpy8(d->text+sum, x->text, x->len - 1); }
  return ApC(ret, putstr(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_u) {
  ArityCheck(3);
  TypeCheck(Argv[0], Str);
  TypeCheck(Argv[1], Num);
  TypeCheck(Argv[2], Num);
  str src = getstr(Argv[0]);
  intptr_t lb = getnum(Argv[1]), ub = getnum(Argv[2]);
  lb = max(lb, 0);
  ub = min(ub, src->len-1);
  ub = max(ub, lb);
  size_t words = Width(str) + b2w(ub - lb + 1);
  Have(words);
  str dst = (str) hp;
  hp += words;
  dst->len = ub - lb + 1;
  dst->disp = disp;
  dst->mtbl = mtbl_str;
  dst->text[ub - lb] = 0;
  cpy8(dst->text, src->text + lb, ub - lb);
  return ApC(ret, putstr(dst)); }

Vm(str_u) {
  size_t i = 0,
    bytes = getnum(Argc) + 1,
    words = Width(str) + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (ob x; i < bytes-1; s->text[i++] = getnum(x)) {
    x = Argv[i];
    TypeCheck(x, Num);
    if (x == N0) break; }
  s->text[i] = 0;
  s->disp = disp;
  s->mtbl = mtbl_str;
  s->len = i+1;
  return ApC(ret, putstr(s)); }

Vm(do_str) {
  fputs(((str) ip)->text, stdout);
  return ApC(ret, (ob) ip); }

size_t hash_str(la v, ob _) {
  str s = (str) _;
  return hashb(s->text, s->len); }

void em_str(la v, FILE *o, ob _) {
  str s = (str) _;
  fputc('"', o);
  for (char *t = s->text; *t; fputc(*t++, o))
    if (*t == '"') fputc('\\', o);
  fputc('"', o); }

ob cp_str(la v, ob _, size_t len0, ob *pool0) {
  str src = getstr(_);
  size_t ws = b2w(src->len);
  str dst = bump(v, Width(str) + ws);
  cpyw(dst, src, Width(str) + ws);
  src->disp = (vm*) dst;
  return (ob) dst; }


