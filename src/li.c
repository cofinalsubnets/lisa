#include "i.h"

NoInline enum status gc(O f, mo ip, ob *hp, ob *sp, uintptr_t n) {
  return Pack(), please(f, n) ? li_go(f) : OomError; }

enum status act(O f, mo ip, ob *hp, ob *sp) {
  return gettyp(ip)->does(f, ip, hp, sp); }

enum status li_go(O f) { return
  f->ip->ap(f, f->ip, f->hp, f->sp); }

void li_fin(O f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

NoInline enum status li_ini(O f) {
  memset(f, 0, sizeof(struct carrier));
  const size_t len0 = 1; // a power of 2
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
  if (o) str_ini(o, bs),
         memcpy(o->text, c, bs);
  return o; }
