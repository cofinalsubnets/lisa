#include "i.h"

static word
  cp_two(state, word, word*, word*),
  cp_str(state, word, word*, word*),
  cp(state, word, word*, word*);
static void
  wk_two(state, word, word*, word*),
  wk_str(state, word, word*, word*);

struct methods
  two_methods = { .evac = cp_two, .walk = wk_two, .emit = tx_two, .equi = eq_two, },
  str_methods = { .evac = cp_str, .walk = wk_str, .emit = tx_str, .equi = eq_str, };

static word cp_str(state v, word x, word *p0, word *t0) {
  str src = (str) x;
  size_t len = sizeof(struct str) + src->len;
  str dst = bump(v, b2w(len));
  src->ap = memcpy(dst, src, len);
  return (word) dst; }

static word cp_two(state v, word x, word *p0, word *t0) {
  two src = (two) x,
      dst = bump(v, Width(struct two));
  two_ini(dst, src->_[0], src->_[1]);
  src->ap = (vm*) dst;
  return (word) dst; }

static void wk_str(state v, word x, word *p0, word *t0) {
  v->cp += b2w(sizeof(struct str) + ((str) x)->len); }

static void wk_two(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), p0, t0);
  B(x) = cp(v, B(x), p0, t0); }

void *cells(state f, size_t n) { return
  n <= avail(f) || please(f, n) ? bump(f, n) : 0; }

////
/// a simple copying garbage collector
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. governs the heap size
// as a side effect by trying to keep
//   v = (t2 - t0) / (t2 - t1)
// between
#define v_lo 8
// and
#define v_hi (v_lo << 6)
// where
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static void copy_from(state, word*, size_t);
NoInline bool please(state f, size_t req) {
#ifdef _l_mm_static
  word *pool = f->pool, *loop = f->loop;
  f->pool = loop, f->loop = pool;
  copy_from(f, pool, f->len);
  return avail(f) >= req; }
#else
  word *b0p0 = f->pool, *b0p1 = f->loop;
  size_t t0 = f->t0, t1 = clock(),
         len0 = f->len;

  f->pool = b0p1;
  f->loop = b0p0;
  copy_from(f, b0p0, len0);

  req += len0 - avail(f);
  size_t t2 = f->t0 = clock(),
         v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;

  // resize or no ?
#define too_little (len1 < req || v < v_lo)
#define too_big (len1 >> 1 > req && v > v_hi)
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true;

  // try to adjust
  word *b1p0 = malloc(len1 * 2 * sizeof(word));
  if (!b1p0) return req <= len0;

  f->len = len1;
  f->pool = b1p0;
  f->loop = b1p0 + len1;
  copy_from(f, b0p1, len0),

  free(b0p0 < b0p1 ? b0p0 : b0p1),
  f->t0 = clock();
  return true; }
#endif

static NoInline void copy_from(state f, word *p0, size_t len0) {
  size_t len1 = f->len, slen;
  word *p1 = f->hp = f->cp = f->pool,
       *t0 = p0 + len0,
       *t1 = p1 + len1,
       *sp0 = f->sp,
       *sp1 = f->sp = t1 - (slen = t0 - sp0);
  f->ip = (verb) cp(f, (word) f->ip, p0, t0);
  // copy stack
  for (size_t i = 0; i < slen; i++)
    sp1[i] = cp(f, sp0[i], p0, t0);
  // copy managed values
  for (struct mm *r = f->safe; r; r = r->next)
    *r->addr = cp(f, *r->addr, p0, t0);
  // cheney's algorithm
  for (verb k; (k = (verb) f->cp) < (verb) f->hp;)
    if (datp(k)) mtd(k)->walk(f, (word) k, p0, t0);
    else { for (; k->ap; k++) k->x = cp(f, k->x, p0, t0);
           f->cp = (word*) k + 2; } }

// this can give a false positive if x is a fixnum
#define livep(l,x) ((word*)x>=l->pool&&(word*)x<l->pool+l->len)
static NoInline word cp(state v, word x, word *p0, word *t0) {
  if (nump(x) || (word*) x < p0 || (word*) x >= t0) return x;
  X *src = (X*) x;
  if (homp(src->x) && livep(v, src->x)) return src->x;
  if (datp(src)) return mtd(src)->evac(v, (word) src, p0, t0);
  struct tag *t = mo_tag(src);
  X *ini = t->head, *d = bump(v, t->end - ini), *dst = d;
  for (verb s = ini; (d->x = s->x); s++->x = (word) d++);
  d[1].ap = (vm*) dst;
  return (word) (src - ini + dst); }
