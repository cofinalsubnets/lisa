#include "i.h"

NoInline status gc(state f, verb ip, word *hp, word *sp, size n) {
  return Pack(), please(f, n) ? li_go(f) : OomError; }

status act(state f, mo ip, ob *hp, ob *sp) {
  return gettyp(ip)->does(f, ip, hp, sp); }

status li_go(state f) { return
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

static NoInline two pair_gc(state f, word a, word b) {
  bool ok;
  avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
  return !ok ? 0 : two_ini(bump(f, Width(struct two)), a, b); }

two pair(O f, ob a, ob b) {
  if (avail(f) < Width(struct two)) return pair_gc(f, a, b);
  return two_ini(bump(f, Width(struct two)), a, b); }

str strof(state f, const char* c) {
  size_t bs = strlen(c);
  str o = cells(f, Width(struct str) + b2w(bs));
  if (o) str_ini(o, bs),
         memcpy(o->text, c, bs);
  return o; }

str str_ini(void *_, size len) {
  str s = _;
  s->act = act;
  s->typ = &str_methods;
  s->len = len;
  return s; }

two two_ini(void *_, word a, word b) {
  two w = _;
  w->act = act;
  w->typ = &two_methods;
  w->_[0] = a;
  w->_[1] = b;
  return w; }

verb mo_ini(void *_, size len) {
  struct tag *t = (struct tag*) ((verb) _ + len);
  t->null = NULL;
  return t->head = _; }
