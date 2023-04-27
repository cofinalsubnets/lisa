#include "i.h"

////
/// a simple copying garbage collector
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. governs the heap size
// as a side effect by trying to keep
//   vim = (t2 - t0) / (t2 - t1)
// between
#define vim_inf 8
// and
#define vim_sup (vim_inf << 6)
// where
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static void copy_from(O, ob*, ob*);
NoInline bool please(O v, uintptr_t req) {
  size_t t1 = clock(), t0 = v->t0, have = v->len;
  ob *pool = v->pool, *loop = v->loop;
  v->pool = loop, v->loop = pool;
  copy_from(v, pool, pool + have);
  size_t t2 = v->t0 = clock(),
         vim = t2 == t1 ? vim_sup : (t2 - t0) / (t2 - t1),
         want = have,
         need = have - (avail(v) - req);

  // if too slow or small then grow
  if (want < need || vim < vim_inf)
    do want <<= 1, vim <<= 1;
    while (want < need || vim < vim_inf);

  // else if too big and fast then shrink
  else if (want >> 1 > need && vim > vim_sup)
    do want >>= 1, vim >>= 1;
    while (want >> 1 > need && vim > vim_sup);

  // else no resize is needed, so return success
  else return true;

  // try and resize
  //
  // first allocate a new pool
  ob *new = malloc(want * 2 * sizeof(ob));

  // if that fails, succeed iff the first copy is big enough
  if (!new) return need <= have;

  // we got a new pool.
  // copy again, free the old pool, return ok
  return v->loop = (v->pool = new) + (v->len = want),
         copy_from(v, loop, loop + have),
         free(pool < loop ? pool : loop),
         v->t0 = clock(),
         true; }

static ob cp(O, ob, ob*, ob*);
static NoInline void copy_from(O v, ob *pool0, ob *top0) {
  size_t len1 = v->len;
  ob *sp0 = v->sp,
     *pool1 = v->pool,
     shift = pool1 + len1 - top0;

  // reset heap
  v->hp = v->cp = v->pool = pool1;
  // copy stack
  for (ob *sp1 = v->sp = sp0 + shift; sp0 < top0;)
    *sp1++ = cp(v, *sp0++, pool0, top0);
  // copy registers
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);
  // copy user values
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);
  // cheney's algorithm
  for (mo k; (k = (mo) v->cp) < (mo) v->hp;)
    if (datp(k)) gettyp(k)->walk(v, (ob) k, pool0, top0);
    else {
      for (; k->ap; k++) k->x = cp(v, k->x, pool0, top0);
      v->cp = (ob*) k + 2; } }

static NoInline ob cp_mo(li v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (d->x = s->x); s++->x = (ob) d++);
  return (d+1)->ap = (void*) dst,
         (ob) (src - ini + dst); }

static NoInline ob cp(li v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  mo src = (mo) x;
  x = (ob) src->ap;
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    gettyp(src)->evac(v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }

ob cp_str(li v, ob x, ob *pool0, ob *top0) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  return memcpy(dst, src, sizeof(struct str) + src->len),
         src->act = (vm*) dst,
         (ob) dst; }

ob cp_two(li v, ob x, ob *pool0, ob *top0) {
  two src = (two) x, dst = bump(v, Width(struct two));
  return src->act = (vm*) dst,
         (ob) two_ini(dst, src->_[0], src->_[1]); }

void wk_str(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

void wk_two(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }
