#include "la.h"
#include <string.h>

str str_ini(void *_, size_t len) {
  str s = _;
  s->data = data, s->typ = &str_typ;
  s->len = len;
  return s; }

static I hx_str(la v, ob _) {
  str s = (str) _;
  I h = 1;
  U words = s->len / sizeof(ob),
    bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const I *ws = (I*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

SI u1 escapep(char c) { return c == '\\' || c == '"'; }

static u0 tx_str(struct carrier *v, FILE *o, ob _) {
  str s = (str) _;
  U len = s->len;
  const char *text = s->text;
  putc('"', o);
  while (len--) {
    char c = *text++;
    if (escapep(c)) putc('\\', o);
    putc(c, o); }
  putc('"', o); }

static Gc(cp_str) {
  str src = (str) x;
  return (ob) (src->data = (vm*)
    memcpy(bump(v, wsizeof(struct str) + b2w(src->len)),
      src, sizeof(struct str) + src->len)); }

static u1 eq_str(struct carrier *v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len &&
    !strncmp(a->text, b->text, a->len); }

static vm ap_str;

const struct typ str_typ = {
  .does = ap_str,
  .emit = tx_str,
  .evac = cp_str,
  .hash = hx_str,
  .equi = eq_str, };

static Vm(ap_str) {
  str s = (str) ip;
  la_putsn(s->text, s->len, stdout);
  return ApC(ret, (ob) ip); }

// string instructions
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
  size_t sum = 0, i = 0;
  for (size_t l = fp->argc; i < l;) {
    ob x = fp->argv[i++];
    Check(strp(x));
    sum += ((str)x)->len; }
  size_t words = wsizeof(struct str) + b2w(sum);
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
  ArityCheck(2);
  Check(strp(fp->argv[0]));
  str src = (str) fp->argv[0];
  intptr_t
    lb = getnum(fp->argv[1]),
    ub = fp->argc > 2 ? getnum(fp->argv[2]) : INTPTR_MAX;
  lb = max(lb, 0);
  ub = min(ub, src->len);
  ub = max(ub, lb);
  U len = ub - lb,
    words = wsizeof(struct str) + b2w(len);
  Have(words);
  str dst = str_ini(hp, len);
  hp += words;
  memcpy(dst->text, src->text + lb, len);
  return ApC(ret, (ob) dst); }

Vm(str_f) {
  U len = fp->argc,
    words = wsizeof(struct str) + b2w(len);
  Have(words);
  str s = str_ini(hp, len);
  hp += words;
  while (len--) s->text[len] = getnum(fp->argv[len]);
  return ApC(ret, (ob) s); }
