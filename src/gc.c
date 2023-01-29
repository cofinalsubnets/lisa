#include "i.h"

ob *new_pool(size_t n) {
  return malloc(n * sizeof(ob)); }
      
////
/// garbage collector
//
static size_t copy(la, size_t);
#define MinVim 32
#define MaxVim (MinVim<<2)
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool.
NoInline bool please(struct V *v, size_t req) {
  // copy into a new pool of the same size.
  size_t have = v->len, vim = copy(v, have);
  if (!vim) return 0;
  size_t want = have, need = have - (Avail - req);

  // grow if we're too slow or small
  if (vim < MinVim || need > want)
    do want <<= 1, vim <<= 1;
    while (vim < MinVim || need > want || vim < MinVim);

  // shrink if we're too big and fast
  else if (vim > MaxVim && need < want >> 1)
    do want >>= 1, vim >>= 1;
    while (vim > MaxVim && need < want >> 1);

  return want == have || copy(v, want) || need <= have; }

// copy : la_clock_t la size_t
// relocate all reachable data into a newly allocated
// memory pool of the given length. return 0 if a new
// pool can't be allocated or else a positive integer
// value u that's higher the less time we spend in GC:
//
//   u = t1 == t2 ? 1 : (t2 - t0) / (t2 - t1)
//
// where
//
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)

static NoInline size_t copy(li v, size_t len1) {
  size_t t2, t1 = clock(), t0 = v->t0;
  ob *pool0 = v->pool,
     *pool1 = new_pool(len1);
  if (!pool1) return 0;

  ob len0 = v->len,
     *sp0 = v->sp,
     *top0 = pool0 + len0,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->len = len1;
  v->hp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (sf) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

  // copy globals
  for (U i = 0; i < Width(struct glob); i++)
    ((ob*)&v->lex)[i] = cp(v, ((ob*)&v->lex)[i], pool0, top0);
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy the stack
  ob *sp = v->sp;
  sf fp = v->fp;
  for (;;) {
    while (sp < (ob*) fp) *sp++ = cp(v, *sp0++, pool0, top0);
    if (sp0 == top0) break;
    sf fp0 = (sf) sp0;
    fp->argc = fp0->argc;
    fp->subd = (sf) ((ob*) fp0->subd + shift);
    fp->clos = (ob*) cp(v, (ob) fp0->clos, pool0, top0);
    fp->retp = (mo) cp(v, (ob) fp0->retp, pool0, top0);
    sp = fp->argv;
    sp0 = fp0->argv;
    fp = fp->subd; }

  return
    free(pool0),
    t2 = v->t0 = clock(),
    t1 = t2 - t1,
    t1 ? (t2 - t0) / t1 : MaxVim; }

#define stale(o) ((ob*)(o) >= pool0 && (ob*) o < top0)
NoInline ob cp(la v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;

  mo src = (mo) x;
  x = (ob) G(src);
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    ((typ) GF(src))->evac(v, (ob) src, pool0, top0);

  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  for (GF(d) = (vm*) dst; d-- > dst;
    G(d) = (vm*) cp(v, (ob) G(d), pool0, top0));
  return (ob) (src - ini + dst); }

Vm(gc) { size_t req = v->xp; return
  CallOut(req = please(v, req)),
  req ? ApY(ip, xp) : Yield(OomError, xp); }
