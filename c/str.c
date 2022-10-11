#include "la.h"
#include "vm.h"
#include <string.h>

ob string(la v, const char* c) {
  intptr_t bs = 1 + slen(c);
  str o = cells(v, Width(str) + b2w(bs));
  if (!o) return 0;
  o->len = bs;
  o->ext = 0;
  memcpy(o->text, c, bs);
  return putstr(o); }

// string instructions
Vm(slen_u) {
  ArityCheck(1);
  TypeCheck(fp->argv[0], Str);
  return ApC(ret, putnum(getstr(*fp->argv)->len-1)); }

Vm(sget_u) {
  ArityCheck(2);
  TypeCheck(fp->argv[0], Str);
  TypeCheck(fp->argv[1], Num);
  return ApC(ret,
    getnum(fp->argv[1]) < getstr(fp->argv[0])->len-1 ?
      putnum(getstr(fp->argv[0])->text[getnum(fp->argv[1])]) :
      nil); }

Vm(scat_u) {
  intptr_t l = getnum(fp->argc), sum = 0, i = 0;
  while (i < l) {
    ob x = fp->argv[i++];
    TypeCheck(x, Str);
    sum += getstr(x)->len - 1; }
  intptr_t words = Width(str) + b2w(sum+1);
  Have(words);
  str d = (str) hp;
  hp += words;
  d->len = sum + 1;
  d->ext = 0;
  d->text[sum] = 0;
  for (str x; i;)
    x = getstr(fp->argv[--i]),
    sum -= x->len - 1,
    memcpy(d->text+sum, x->text, x->len - 1);
  return ApC(ret, putstr(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_u) {
  ArityCheck(3);
  TypeCheck(fp->argv[0], Str);
  TypeCheck(fp->argv[1], Num);
  TypeCheck(fp->argv[2], Num);
  str src = getstr(fp->argv[0]);
  intptr_t lb = getnum(fp->argv[1]),
           ub = getnum(fp->argv[2]);
  lb = max(lb, 0);
  ub = min(ub, src->len-1);
  ub = max(ub, lb);
  intptr_t words = Width(str) + b2w(ub - lb + 1);
  Have(words);
  str dst = (str) hp;
  hp += words;
  dst->len = ub - lb + 1;
  dst->ext = 0;
  dst->text[ub - lb] = 0;
  memcpy(dst->text, src->text + lb, ub - lb);
  return ApC(ret, putstr(dst)); }

Vm(str_u) {
  intptr_t i = 0,
    bytes = getnum(fp->argc)+1,
    words = Width(str) + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (ob x; i < bytes-1; s->text[i++] = getnum(x)) {
    x = fp->argv[i];
    TypeCheck(x, Num);
    if (x == N0) break; }
  s->text[i] = 0;
  s->ext = 0;
  s->len = i+1;
  return ApC(ret, putstr(s)); }
