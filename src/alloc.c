#include "la.h"
#include "gc.h"
#include "alloc.h"

// general memory allocation / access functions

// unchecked allocator -- make sure there's enough memory!
void *bump(la v, size_t n) {
  void *x = v->hp;
  v->hp += n;
  return x; }

void *cells(la v, size_t n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

ob *alloc_pool(la_carrier v, size_t n) {
  return calloc(n, sizeof(ob)); }

#include "mo.h"
#include <stdarg.h>

// push things onto the stack
static NoInline bool pushsr(la v, size_t i, va_list xs) {
  ob x = va_arg(xs, ob);
  if (!x) return Avail >= i || please(v, i);
  bool _;
  with(x, _ = pushsr(v, i+1, xs));
  return _ && (*--v->sp = x, true); }

bool pushs(la v, ...) {
  va_list xs;
  va_start(xs, v);
  bool _ = pushsr(v, 0, xs);
  va_end(xs);
  return _; }

static NoInline ob *tuplr(la v, size_t i, va_list xs) {
  ob *k, x = va_arg(xs, ob);
  if (!x) return
    k = (ob*) mkmo(v, i),
    k ? k + i : k;
  with(x, k = tuplr(v, i+1, xs));
  return !k ? k : (*--k = x, k); }

ob tupl(la v, ...) {
  va_list xs;
  va_start(xs, v);
  ob *k = tuplr(v, 0, xs);
  va_end(xs);
  return (ob) k; }
