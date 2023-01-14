#include "i.h"

str str_ini(void *_, size_t len) {
  str s = _; return
    s->act = act, s->typ = &str_typ,
    s->len = len,
    s; }

static intptr_t hx_str(la v, ob _) {
  str s = (str) _;
  intptr_t h = 1;
  size_t words = s->len / sizeof(ob),
         bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const I *ws = (I*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static Inline bool escapep(char c) { return c == '\\' || c == '"'; }

static void tx_str(struct V *v, FILE *o, ob _) {
  str s = (str) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if (escapep(c = *text++)) putc('\\', o);
  putc('"', o); }

Gc(cp_str) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  return (ob) (src->act = (vm*) dst); }

static bool eq_str(struct V *v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }

static Vm(ap_str) {
  str s = (str) ip;
  fputsn(s->text, s->len, stdout);
  return ApC(ret, (ob) ip); }

const struct typ str_typ = {
  .actn = ap_str,
  .emit = tx_str,
  .evac = cp_str,
  .hash = hx_str,
  .equi = eq_str, };

// string instructions
Vm(slen_f) { return
  fp->argc == 0 ? Yield(ArityError, putnum(1)) :
  !strp(xp = fp->argv[0]) ? Yield(DomainError, xp) :
  ApC(ret, putnum(((str) xp)->len)); }

Vm(sget_f) {
  if (fp->argc < 2) return Yield(ArityError, putnum(2));
  if (!strp(fp->argv[0])) return Yield(DomainError, xp);
  str s = (str) fp->argv[0];
  I i = getnum(fp->argv[1]);
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
  I lb = getnum(fp->argv[1]),
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
