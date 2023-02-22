#include "i.h"

str strof(li v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  if (o) memcpy(str_ini(o, bs)->text, c, bs);
  return o; }

// string instructions
Vm(slen_f) { return
  fp->argc == 0 ? Yield(ArityError, putnum(1)) :
  !strp(xp = fp->argv[0]) ? Yield(DomainError, xp) :
  ApC(ret, putnum(((str) xp)->len)); }

Vm(sget_f) {
  if (fp->argc < 2) return Yield(ArityError, putnum(2));
  if (!strp(fp->argv[0])) return Yield(DomainError, xp);
  str s = (str) fp->argv[0];
  intptr_t i = getnum(fp->argv[1]);
  xp = i < 0 || i >= s->len ? nil : putnum(s->text[i]);
  return ApC(ret, xp); }

Vm(scat_f) {
  size_t sum = 0, i = 0;
  for (size_t l = fp->argc; i < l;) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str)x)->len; }
  size_t words = Width(struct str) + b2w(sum);
  Have(words);
  str d = str_ini(hp, sum);
  hp += words;
  for (str x; i--;
    x = (str) fp->argv[i],
    sum -= x->len,
    memcpy(d->text+sum, x->text, x->len));
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_f) {
  if (fp->argc < 2) return Yield(ArityError, putnum(2));
  if (!strp(fp->argv[0])) return Yield(DomainError, xp);
  str src = (str) fp->argv[0];
  intptr_t lb = getnum(fp->argv[1]),
           ub = fp->argc > 2 ? getnum(fp->argv[2]) : INTPTR_MAX;
  lb = max(lb, 0);
  ub = min(ub, src->len);
  ub = max(ub, lb);
  size_t len = ub - lb,
         words = Width(struct str) + b2w(len);
  Have(words);
  str dst = str_ini(hp, len);
  hp += words;
  memcpy(dst->text, src->text + lb, len);
  return ApC(ret, (ob) dst); }

Vm(str_f) {
  size_t len = fp->argc,
         words = Width(struct str) + b2w(len);
  Have(words);
  str s = str_ini(hp, len);
  hp += words;
  while (len--) s->text[len] = getnum(fp->argv[len]);
  return ApC(ret, (ob) s); }
