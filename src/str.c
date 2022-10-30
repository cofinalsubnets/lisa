#include "la.h"
#include "vm.h"
#include <string.h>

ob string(la v, const char* c) {
  size_t bs = 1 + strlen(c);
  str o = cells(v, Width(str) + b2w(bs));
  if (!o) return 0;
  o = ini_str(o, bs);
  memcpy(o->text, c, bs);
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
  size_t sum = 0, i = 0, l = fp->argc;
  while (i < l) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str) x)->len - 1; }
  size_t words = Width(str) + b2w(sum+1);
  Have(words);
  str d = ini_str(hp, sum + 1);
  hp += words;
  d->text[sum] = 0;
  for (str x; i--;
    x = ((str) fp->argv[i]),
    sum -= x->len - 1,
    memcpy(d->text+sum, x->text, x->len - 1));
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_u) {
  ArityCheck(3);
  Check(strp(fp->argv[0]));
  Check(nump(fp->argv[1]));
  Check(nump(fp->argv[2]));
  str src = (str) fp->argv[0];
  intptr_t lb = getnum(fp->argv[1]), ub = getnum(fp->argv[2]);
  lb = max(lb, 0);
  ub = min(ub, src->len-1);
  ub = max(ub, lb);
  size_t words = Width(str) + b2w(ub - lb + 1);
  Have(words);
  str dst = ini_str(hp, ub - lb + 1);
  hp += words;
  dst->text[ub - lb] = 0;
  memcpy(dst->text, src->text + lb, ub - lb);
  return ApC(ret, (ob) dst); }

Vm(str_u) {
  size_t i = 0,
    bytes = fp->argc + 1,
    words = Width(str) + b2w(bytes);
  Have(words);
  str s = (str) hp;
  hp += words;
  for (; i < bytes-1; s->text[i++] = xp)
    if (!(xp = getnum(fp->argv[i]))) break;
  s->text[i] = 0;
  s->disp = disp;
  s->mtbl = mtbl_str; // FIXME
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
  str src = (str) x, dst = bump(v, Width(str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  return (ob) (src->disp = (vm*) dst); }

static bool eq_str(la v, ob x, ob y) {
  return strp(y) && 0 == strcmp(((str) x)->text, ((str) y)->text); }

const struct mtbl s_mtbl_str = {
  .does = do_str,
  .emit = em_str,
  .copy = cp_str,
  .hash = hash_str,
  .equi = eq_str, };
