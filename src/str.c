#include "la.h"
#include "alloc.h"
#include "str.h"
#include <string.h>

str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(str) + b2w(bs));
  if (o) memcpy(o->text, c, bs),
         ini_str(o, bs);
  return o; }

// string instructions
#include "vm.h"
Vm(slen_f) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(strp(xp));
  return ApC(ret, putnum(((str)xp)->len)); }

Vm(sget_f) {
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  str s = (str) fp->argv[0];
  intptr_t i = getnum(fp->argv[1]);
  xp = i < 0 || i >= s->len ? nil : putnum(s->text[i]);
  return ApC(ret, xp); }

Vm(scat_f) {
  size_t sum = 0, i = 0, l = fp->argc;
  while (i < l) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str)x)->len; }
  size_t words = Width(str) + b2w(sum);
  Have(words);
  str d = ini_str(hp, sum);
  hp += words;
  for (str x; i--;
    x = (str) fp->argv[i],
    sum -= x->len,
    memcpy(d->text+sum, x->text, x->len));
  return ApC(ret, (ob) d); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(ssub_f) {
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  str src = (str) fp->argv[0];
  intptr_t
    lb = getnum(fp->argv[1]),
    ub = fp->argc > 2 ? getnum(fp->argv[2]) : INTPTR_MAX;
  lb = max(lb, 0);
  ub = min(ub, src->len);
  ub = max(ub, lb);
  size_t len = ub - lb,
         words = Width(str) + b2w(len);
  Have(words);
  str dst = ini_str(hp, len);
  hp += words;
  memcpy(dst->text, src->text + lb, len);
  return ApC(ret, (ob) dst); }

Vm(str_f) {
  size_t len = fp->argc,
         words = Width(str) + b2w(len);
  Have(words);
  str s = ini_str(hp, len);
  hp += words;
  while (len--) s->text[len] = getnum(fp->argv[len]);
  return ApC(ret, (ob) s); }

#include "tx.h"
static Vm(apstr) {
  str s = (str) ip;
  fputstr(stdout, s);
  return ApC(ret, (ob) ip); }

static intptr_t hxstr(la v, ob _) {
  str s = (str) _;
  intptr_t h = 1;
  size_t words = s->len / sizeof(ob),
         bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (void*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static Inline bool escapep(char c) {
  return c == '\\' || c == '"'; }

static long txstr(la v, FILE *o, ob _) {
  str s = (str) _;
  size_t len = s->len;
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

#include "gc.h"
static Gc(cpstr) {
  str src = (str) x,
      dst = bump(v, Width(str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  src->head.disp = (vm*) dst;
  return (ob) dst; }

static bool eqstr(la v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len &&
    !strncmp(a->text, b->text, a->len); }

const struct mtbl mtbl_str = {
  .does = apstr,
  .emit = txstr,
  .evac = cpstr,
  .hash = hxstr,
  .equi = eqstr, };
