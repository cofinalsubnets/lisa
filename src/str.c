#include "i.h"

str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  if (!o) return 0;
  memcpy(o->text, c, bs);
  return str_ini(o, bs); }

static uintptr_t hx_str(la v, ob _) {
  str s = (str) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(ob),
         bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const I *ws = (I*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static Inline bool escapep(char c) {
  return c == '\\' || c == '"'; }

static void tx_str(struct V *v, FILE *o, ob _) {
  str s = (str) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if (escapep(c = *text++)) putc('\\', o);
  putc('"', o); }

static Gc(cp_str) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  return
    memcpy(dst, src, sizeof(struct str) + src->len),
    (ob) (src->act = (vm*) dst); }

static bool eq_str(struct V *v, ob x, ob y) {
  if (!strp(y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }

const struct typ str_typ = {
  .actn = immk,
  .emit = tx_str,
  .evac = cp_str,
  .hash = hx_str,
  .equi = eq_str, };
