#include "i.h"

static void wk(li, ob*, ob*);
////
/// garbage collector
//
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool by trying to keep
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

  // grow if we're too slow or small
  if (want < need || vim < MinVim)
    do want <<= 1, vim <<= 1;
    while (want < need || vim < MinVim);

  // shrink if we're too big and fast
  else if (want >> 1 > need && vim > MaxVim)
    do want >>= 1, vim >>= 1;
    while (want >> 1 > need && vim > MaxVim);

  else return true;

  ob *new = new_pool(want);
  if (!new) return need <= have;

  v->len = want, v->pool = new, v->loop = new + want;
  copy_from(v, loop, loop + have);
  free(pool < loop ? pool : loop);
  v->t0 = clock();
  return true; }

static NoInline void copy_from(li v, ob *pool0, ob *top0) {
  size_t len1 = v->len;
  ob *sp0 = v->sp,
     *pool1 = v->pool,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->len = len1;
  v->hp = v->cp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (sf) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

  // copy globals
  v->lex = (struct glob*) cp(v, (ob) v->lex, pool0, top0);
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy the stack
  ob *sp = v->sp;
  frame fp = v->fp;
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

  // while (v->cp < v->hp) wk(v, pool0, top0);
}

ob *new_pool(size_t n) {
  return malloc(n * 2 * sizeof(ob)); }

static NoInline ob cp_mo(li v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  for (GF(d) = (vm*) dst; d-- > dst;
    G(d) = (vm*) cp(v, (ob) G(d), pool0, top0));
  return (ob) (src - ini + dst); }

static NoInline void wk_mo(li v, mo src, ob *pool0, ob *top0) {
  for (; G(src); src++)
    G(src) = (vm*) cp(v, (ob) G(src), pool0, top0);
  v->cp = (ob*) src + 2; }

static void wk(li v, ob *pool0, ob *top0) {
  mo k = (mo) v->cp;
  if (G(k) == act) ((typ) GF(k))->walk(v, (ob) k, pool0, top0);
  else wk_mo(v, k, pool0, top0); }

NoInline ob cp(la v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  mo src = (mo) x;
  x = (ob) G(src);
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    ((typ) GF(src))->evac(v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }
