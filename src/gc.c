#include "i.h"

static void copy_from(li, ob*, ob*);

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

static NoInline void copy_from(li v, ob *pool0, ob *top0) {
  size_t len1 = v->len;
  ob *sp0 = v->sp,
     *pool1 = v->pool,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->hp = v->cp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (sf) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

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

  // copy saved values
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy globals
  v->lex = (struct glob*) cp(v, (ob) v->lex, pool0, top0);

  // cheney's algorithm
  while (v->cp < v->hp) {
    mo k = (mo) v->cp;
    if (G(k) == act) gettyp(k)->walk(v, (ob) k, pool0, top0);
    else { // it's a function thread
      for (; G(k); k++) G(k) = (vm*) cp(v, (ob) G(k), pool0, top0);
      v->cp = (ob*) k + 2; } } }


static NoInline ob cp_mo(li v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  return GF(d) = (vm*) dst,
         (ob) (src - ini + dst); }

NoInline ob cp(la v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  mo src = (mo) x;
  x = (ob) G(src);
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    gettyp(src)->evac(v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }

Gc(cp_tbl) {
  tbl src = (tbl) x;
  size_t i = src->cap;
  tbl dst = bump(v, Width(struct tbl) + i);
  src->act = (vm*) dst;
  ini_tbl(dst, src->len, i, (struct tbl_e**) (dst+1));
  for (struct tbl_e *s, *e, *d; i--; dst->tab[i] = e)
    for (s = src->tab[i], e = NULL; s;
      d = bump(v, Width(struct tbl_e)),
      d->key = s->key, d->val = s->val,
      d->next = e, e = d,
      s = s->next);
  return (ob) dst; }

Gc(cp_sym) {
  sym src = (sym) x,
      dst = src->nom ?
        intern(v, &v->syms, (str) cp(v, (ob) src->nom, pool0, top0)) :
        ini_anon(bump(v, Width(struct sym) - 2), src->code);
  return (ob) (src->act = (vm*) dst); }

Gc(cp_str) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  src->act = (vm*) dst;
  return (ob) dst; }

Gc(cp_two) {
  two src = (two) x,
      dst = bump(v, Width(struct two));
  src->act = (vm*) dst;
  return (ob) two_ini(dst, src->a, src->b); }
