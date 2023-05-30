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
#define big (want < need || vim < vim_inf)
#define little (want >> 1 > need && vim > vim_sup)
static void copy_from(state, word*, word*);
NoInline bool please(state v, size req) {
  size t1 = clock(), t0 = v->t0, have = v->len;
  ob *pool = v->pool, *loop = v->loop;
  v->pool = loop, v->loop = pool;
  copy_from(v, pool, pool + have);
  size t2 = v->t0 = clock(),
       vim = t2 == t1 ? vim_sup : (t2 - t0) / (t2 - t1),
       want = have,
       need = have - (avail(v) - req);
  if         (big) do want <<= 1, vim <<= 1; while (big);
  else if (little) do want >>= 1, vim >>= 1; while (little);
  else return true; // else no resize is needed, so return success
  // try and resize
  //
  ob *new = malloc(want * 2 * sizeof(ob)); // allocate a new pool
  if (!new) return need <= have; // if that fails, succeed iff the first copy is big enough
  // we got a new pool; copy again, free the old pool, return ok
  v->loop = (v->pool = new) + (v->len = want);
  copy_from(v, loop, loop + have);
  free(pool < loop ? pool : loop);
  v->t0 = clock();
  return true; }

static word cp(state, word, word*, word*);
static NoInline void copy_from(state v, word *pool0, word *top0) {
  size len1 = v->len;
  word *sp0 = v->sp,
       *pool1 = v->pool,
       *top1 = pool1 + len1;
  size slen = top0 - sp0;
  // reset heap
  v->hp = v->cp = pool1;
  // copy stack
  word *sp1 = v->sp = top1 - slen;
  for (size i = 0; i < slen; i++)
    sp1[i] = cp(v, sp0[i], pool0, top0);
  // copy registers
  v->ip = (verb) cp(v, (word) v->ip, pool0, top0);
  // copy user values
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);
  // cheney's algorithm
  for (mo k; (k = (mo) v->cp) < (mo) v->hp;)
    if (datp(k)) gettyp(k)->walk(v, (ob) k, pool0, top0);
    else { for (; k->ap; k++) k->x = cp(v, k->x, pool0, top0);
           v->cp = (ob*) k + 2; } }

static NoInline word cp_mo(state v, verb src, word *pool0, word *top0) {
  struct tag *fin = mo_tag(src);
  verb ini = fin->head,
       dst = bump(v, fin->end - ini),
       d = dst;
  for (verb s = ini; (d->x = s->x); s++->x = (ob) d++);
  return (d+1)->ap = (void*) dst,
         (ob) (src - ini + dst); }

static NoInline word cp(state v, word x, word *pool0, word *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  verb src = (mo) x;
  if (homp(src->x) && livep(v, src->x)) return src->x;
  if (src->ap == act) return gettyp(src)->evac(v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }

word cp_str(state v, word x, word *pool0, word *top0) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  return memcpy(dst, src, sizeof(struct str) + src->len),
         src->act = (void*) dst,
         (ob) dst; }

word cp_two(state v, word x, word *pool0, word *top0) {
  two src = (two) x, dst = bump(v, Width(struct two));
  return src->act = (void*) dst,
         (ob) two_ini(dst, src->_[0], src->_[1]); }

void wk_str(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

void wk_two(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }
