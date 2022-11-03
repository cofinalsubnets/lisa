#include "la.h"
#include <string.h>

// FIXME remove null terminators

str strof(la v, const char* c) {
  size_t bs = 1 + strlen(c); // XXX null terminated
  str o = cells(v, Width(str) + b2w(bs));
  if (!o) return 0;
  memcpy(o->text, c, bs);
  return ini_str(o, bs); }

// string instructions
Vm(slen_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(strp(xp));
  return ApC(ret, putnum(((str) xp)->len-1)); } // XXX null terminated

Vm(sget_u) {
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  Check(nump(fp->argv[1]));
  str s = (str) fp->argv[0];
  intptr_t i = getnum(fp->argv[1]);
  xp = i < 0 || i >= s->len ? nil : putnum(s->text[i]);
  return ApC(ret, xp); }

Vm(scat_u) {
  size_t sum = 0, i = 0, l = fp->argc;
  while (i < l) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str) x)->len - 1; } // XXX null terminated
  size_t words = Width(str) + b2w(sum+1); // XXX
  Have(words);
  str d = ini_str(hp, sum + 1); // XXX
  hp += words;
  d->text[sum] = 0; // XXX
  for (str x; i--;
    x = (str) fp->argv[i],
    sum -= x->len - 1, // XXX
    memcpy(d->text+sum, x->text, x->len - 1)); // XXX
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_u) {
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  Check(nump(fp->argv[1]));
  str src = (str) fp->argv[0];
  intptr_t lb = getnum(fp->argv[1]), ub = INTPTR_MAX;
  lb = max(lb, 0);
  if (fp->argc > 2) {
    Check(nump(fp->argv[2]));
    ub = getnum(fp->argv[2]); }
  ub = min(ub, src->len-1); // XXX
  ub = max(ub, lb);
  size_t words = Width(str) + b2w(ub - lb + 1); // XXX
  Have(words);
  str dst = ini_str(hp, ub - lb + 1); // XXX
  hp += words;
  dst->text[ub - lb] = 0; // XXX
  memcpy(dst->text, src->text + lb, ub - lb);
  return ApC(ret, (ob) dst); }

Vm(str_u) {
  size_t
    chars = fp->argc,
    words = Width(str) + b2w(chars + 1); // XXX
  Have(words);
  str s = ini_str(hp, chars + 1);
  s->text[chars] = 0; // XXX
  hp += words;
  while (chars--)
    s->text[chars] = getnum(fp->argv[chars]);
  return ApC(ret, (ob) s); }

static Vm(ap_str) {
  str s = (str) ip;
  fputstr(stdout, s);
  return ApC(ret, (ob) ip); }

// FIXME make faster
static intptr_t hx_str(la v, ob _) {
  str s = (str) _;
  const char *bs = s->text;
  intptr_t h = 1, n = s->len;
  while (n--) h = (h ^ (mix * *bs++)) * mix;
  return h; }

static bool escapep(char c) {
  return c == '\\' || c == '"'; }

static long em_str(la v, FILE *o, ob _) {
  str s = (str) _;
  size_t len = s->len - 1; // XXX null
  const char *text = s->text;
  long r = len + 2;
  if (fputc('"', o) == EOF) return -1;
  while (len--) {
    char c = *text++;
    if (escapep(c)) {
      if (fputc('\\', o) == EOF) return -1;
      r++; }
    if (fputc(c, o) == EOF) return -1; }
  if (fputc('"', o) == EOF) return -1;
  return r; }

static Gc(cp_str) {
  str src = (str) x, dst = bump(v, Width(str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  return (ob) (src->disp = (vm*) dst); }

static bool eq_str(la v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len &&
    !strncmp(a->text, b->text, a->len); }

const struct mtbl mtbl_str = {
  .does = ap_str,
  .emit = em_str,
  .evac = cp_str,
  .hash = hx_str,
  .equi = eq_str, };
