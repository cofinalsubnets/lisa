#include "i.h"

enum status act(li v) {
  ob t = v->sp[1];
  v->sp[1] = v->sp[0];
  v->sp[0] = t;
  return Ok; }
enum status li_go(O f) { return
  ((enum status (*)(O)) pop1(f))(f); }

void li_fin(O f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

NoInline enum status li_ini(O f) {
  memset(f, 0, sizeof(struct carrier));
  const size_t len0 = 1 << 10; // a power of 2
  ob *pool = malloc(len0 * 2 * sizeof(intptr_t));
  if (!pool) return OomError;
  f->len = len0;
  f->pool = f->hp = pool;
  f->loop = f->sp = pool + len0;
  f->t0 = clock();
  return Ok; }

NoInline two pair(O f, ob a, ob b) {
  if (avail(f) < Width(struct two)) {
    bool ok; avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
    if (!ok) return NULL; }
  return two_ini(bump(f, Width(struct two)), a, b); }

str strof(li v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  if (o) memcpy(str_ini(o, bs)->text, c, bs);
  return o; }
