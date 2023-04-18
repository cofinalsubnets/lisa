#include "i.h"

typedef ob gc_evac(li, ob, ob*, ob*);
static gc_evac cp_str, cp_two,
  *const data_evac[] = {
    [Two] = cp_two,
    [Str] = cp_str, };

typedef void gc_walk(li, ob, ob*, ob*);
static gc_walk wk_str, wk_two,
  *const data_walk[] = {
    [Two] = wk_two,
    [Str] = wk_str, };

ob *new_pool(size_t n) { return malloc(n * 2 * sizeof(ob)); }

////
/// garbage collector
//
// it's a dynamic semispace copying collector that uses
// cheney's algorithm to avoid stack recursion.
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool by attempting to keep
//
//   vim = t1 == t2 ? 1 : (t2 - t0) / (t2 - t1)
//
// between
#define MinVim 8
// and
#define MaxVim (MinVim<<6)
// where
//
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static void copy_from(li, ob*, ob*);
NoInline bool please(li v, size_t req) {
  size_t t1 = clock(), t0 = v->t0, have = v->len;
  ob *pool = v->pool, *loop = v->loop;
  v->pool = loop, v->loop = pool;
  copy_from(v, pool, pool + have);
  size_t t2 = v->t0 = clock(),
         vim = t2 == t1 ? MaxVim : (t2 - t0) / (t2 - t1),
         want = have,
         need = have - (Avail - req);

  // if we're too slow or small then grow
  if (want < need || vim < MinVim)
    do want <<= 1, vim <<= 1;
    while (want < need || vim < MinVim);

  // else if we're too big and fast then shrink
  else if (want >> 1 > need && vim > MaxVim)
    do want >>= 1, vim >>= 1;
    while (want >> 1 > need && vim > MaxVim);

  // else return ok
  else return true;

  // allocate a new pool
  ob *new = new_pool(want);

  // if it fails, succeed iff the first copy is big enough
  if (!new) return need <= have;

  // copy again, free old pool, return ok
  return v->len = want,
         v->pool = new,
         v->loop = new + want,
         copy_from(v, loop, loop + have),
         free(pool < loop ? pool : loop),
         v->t0 = clock(),
         true; }

static ob cp(li, ob, ob*, ob*);
static NoInline void copy_from(li v, ob *pool0, ob *top0) {
  size_t len1 = v->len;
  ob *sp0 = v->sp,
     *pool1 = v->pool,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->hp = v->cp = v->pool = pool1;

  // copy saved values
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy the stack
  ob *sp1 = v->sp = sp0 + shift;
  while (sp0 < top0) *sp1++ = cp(v, *sp0++, pool0, top0);

  // cheney's algorithm
  while (v->cp < v->hp) {
    mo k = (mo) v->cp;
    if (G(k) == act) data_walk[gettyp(k)](v, (ob) k, pool0, top0);
    else { // it's a function thread
      for (; G(k); k++) G(k) = (vm*) cp(v, (ob) G(k), pool0, top0);
      v->cp = (ob*) k + 2; } } }

static NoInline ob cp_mo(li v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  return GF(d) = (vm*) dst, (ob) (src - ini + dst); }

static NoInline ob cp(li v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  mo src = (mo) x;
  x = (ob) G(src);
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    data_evac[gettyp(src)](v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }



static ob cp_str(li v, ob x, ob *pool0, ob *top0) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  return memcpy(dst, src, sizeof(struct str) + src->len),
         src->act = (vm*) dst,
         (ob) dst; }

static ob cp_two(li v, ob x, ob *pool0, ob *top0) {
  two src = (two) x, dst = bump(v, Width(struct two));
  return src->act = (vm*) dst,
         (ob) two_ini(dst, src->a, src->b); }

void wk_str(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

void wk_two(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }
