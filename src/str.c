#include "la.h"
#include "vm.h"
#include <string.h>

ob string(la v, const char* c) {
  size_t bs = 1 + slen(c);
  str o = cells(v, Width(str) + b2w(bs));
  if (!o) return 0;
  o->len = bs;
  o->disp = disp;
  o->mtbl = mtbl_str;
  cpy8(o->text, c, bs);
  return (ob) o; }

// string instructions
Vm(slen_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(strp(xp));
  return ApC(ret, putnum(((str) xp)->len-1)); }

Vm(sget_u) {
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  Check(nump(fp->argv[1]));
  return ApC(ret,
    getnum(fp->argv[1]) < ((str) fp->argv[0])->len-1 ?
      putnum(((str) fp->argv[0])->text[getnum(fp->argv[1])]) :
      nil); }

Vm(scat_u) {
  size_t sum = 0, i = 0, l = getnum(fp->argc);
  while (i < l) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str) x)->len - 1; }
  size_t words = Width(str) + b2w(sum+1);
  Have(words);
  str d = (str) hp;
  hp += words;
  d->len = sum + 1;
  d->disp = disp;
  d->mtbl = mtbl_str;
  d->text[sum] = 0;
  while (i) {
    str x = ((str) fp->argv[--i]);
    sum -= x->len - 1;
    cpy8(d->text+sum, x->text, x->len - 1); }
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_u) {
  ArityCheck(3);
  Check(strp(fp->argv[0]));
  Check(nump(fp->argv[1]));
  Check(nump(fp->argv[2]));
  str src = ((str) fp->argv[0]);
  intptr_t lb = getnum(fp->argv[1]), ub = getnum(fp->argv[2]);
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
  return ApC(ret, (ob) dst); }

Vm(str_u) {
  size_t i = 0,
    bytes = getnum(fp->argc) + 1,
    words = Width(str) + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (; i < bytes-1; s->text[i++] = getnum(xp)) {
    xp = fp->argv[i];
    Check(nump(xp));
    if (xp == putnum(0)) break; }
  s->text[i] = 0;
  s->disp = disp;
  s->mtbl = mtbl_str;
  s->len = i + 1;
  return ApC(ret, (ob) s); }

static Vm(do_str) { return
  fputs(((str) ip)->text, stdout),
  ApC(ret, (ob) ip); }

static size_t hash_str(la v, ob _) {
  return hashb(((str)_)->text, ((str)_)->len); }

static int em_str(la v, FILE *o, ob _) {
  int r = 2;
  fputc('"', o);
  for (char *t = ((str)_)->text; *t; fputc(*t++, o), r++)
    if (*t == '"') r++, fputc('\\', o);
  fputc('"', o);
  return r; }

static Gc(cp_str) {
  str src = (str) x;
  size_t ws = b2w(src->len);
  str dst = bump(v, Width(str) + ws);
  cpyw(dst, src, Width(str) + ws);
  src->disp = (vm*) dst;
  return (ob) dst; }

bool strp(ob _) { return homp(_) && GF(_) == (vm*) mtbl_str; }

const struct mtbl s_mtbl_str = {
  .does = do_str,
  .emit = em_str,
  .copy = cp_str,
  .hash = hash_str,
};

