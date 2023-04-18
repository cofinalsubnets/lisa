#include "i.h"

enum status act(li v) {
  ob t = v->sp[1];
  v->sp[1] = v->sp[0];
  v->sp[0] = t;
  return Ok; }

ob pop1(li v) { return *v->sp++;}
static NoInline ob push1_gc(li v, ob x) {
  bool ok; return with(x, ok = please(v, 1)),
                  ok ? (*--v->sp = x) : 0; }
ob push1(li v, ob x) {
  return Avail ? (*--v->sp = x) : push1_gc(v, x); }

// push things onto the stack
static NoInline bool pushsr(li v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i + 1, xs)),
     _ && (*--v->sp = x, true)); }

NoInline bool pushs(li v, ...) {
  bool _; va_list xs; return
    va_start(xs, v),
    _ = pushsr(v, 0, xs),
    va_end(xs),
    _; }

enum status li_go(li v) {
  return ((enum status (*)(li))*v->sp++)(v); }


void li_fin(li v) { if (v)
  free(v->pool < v->loop ? v->pool : v->loop),
  v->pool = v->loop = NULL; }


// initialize a state
NoInline enum status li_ini(li v) {
  memset(v, 0, sizeof(struct V));
  const size_t len = 1 << 10; // a power of 2
  ob _, *pool = new_pool(len);
  struct glob *l; struct sym *y;
  if (!pool) return OomError;
  v->len = len,
  v->pool = v->hp = pool,
  v->loop = pool + len,
  v->sp = pool + len,
  v->t0 = clock();
  return Ok; }
