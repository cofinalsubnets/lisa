#include "i.h"

void *cells(state f, size_t n) { return
  n <= avail(f) || please(f, n) ? bump(f, n) : 0; }

// garbage collector

typedef word gc_copy(state, word, word*, word*);
typedef void gc_evac(state, word, word*, word*);
static gc_copy cp_two, cp_str, cp;
static gc_evac wk_two, wk_str;
static gc_copy *gc_copy_s[] = { [Pair] = cp_two, [String] = cp_str, };
static gc_evac *gc_evac_s[] = { [Pair] = wk_two, [String] = wk_str, };

static word cp_str(state v, word x, word *p0, word *t0) {
  string src = (string) x;
  size_t len = sizeof(struct string) + src->len;
  string dst = bump(v, b2w(len));
  src->ap = memcpy(dst, src, len);
  return (word) dst; }

static word cp_two(state v, word x, word *p0, word *t0) {
  pair src = (pair) x,
       dst = bump(v, Width(struct two));
  dst->ap = data, dst->typ = Pair,
  dst->a = src->a, dst->b = src->b;
  src->ap = (vm*) dst;
  return (word) dst; }

static void wk_str(state v, word x, word *p0, word *t0) {
  v->cp += b2w(sizeof(struct string) + ((string) x)->len); }

static void wk_two(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), p0, t0);
  B(x) = cp(v, B(x), p0, t0); }

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
  word *b0p0 = f->pool, *b0p1 = f->loop;
  f->pool = b0p1, f->loop = b0p0;
#ifdef _gwen_mem_static
  return copy_from(f, b0p0, f->len), avail(f) >= req; }
#else
  size_t t0 = f->t0, t1 = clock(),
         len0 = f->len;

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
  word *b1p0 = l_malloc(len1 * 2 * sizeof(word));
  if (!b1p0) return req <= len0;

  f->len = len1;
  f->pool = b1p0;
  f->loop = b1p0 + len1;
  copy_from(f, b0p1, len0),

  l_free(b0p0 < b0p1 ? b0p0 : b0p1),
  f->t0 = clock();
  return true; }
#endif

static NoInline void copy_from(state f, word *p0, size_t len0) {
  word len1 = f->len,
       *p1 = f->pool,
       *t0 = p0 + len0,
       *t1 = p1 + len1,
       *sp0 = f->sp,
       slen = t0 - sp0,
       *sp1 = t1 - slen;
  f->sp = sp1;
  f->hp = f->cp = p1;
  f->ip = (thread) cp(f, (word) f->ip, p0, t0);
  f->dict = cp(f, f->dict, p0, t0);
  f->macro = cp(f, f->macro, p0, t0);
  // copy stack
  for (size_t i = 0; i < slen; i++)
    sp1[i] = cp(f, sp0[i], p0, t0);
  // copy managed values
  for (struct mm *r = f->safe; r; r = r->next)
    *r->addr = cp(f, *r->addr, p0, t0);
  // cheney's algorithm
  for (thread k; (k = (thread) f->cp) < (thread) f->hp;)
    if (datp(k)) gc_evac_s[k[1].x](f, (word) k, p0, t0);
    else { for (; k->ap; k++) k->x = cp(f, k->x, p0, t0);
           f->cp = (word*) k + 2; }

  assert(f->hp <= f->sp);
  assert(f->sp <= f->pool + f->len); }

static NoInline word cp(state v, word x, word *p0, word *t0) {
  if (nump(x) || x < (word) p0 || x >= (word) t0) return x;
  cell src = (cell) x;
  if (homp(src->x) && // if first item is a pointer
      v->pool <= (word*) src->x && (word*) src->x < v->pool + v->len) // into new space
    return src->x; // then return the pointer
  if (datp(src)) return gc_copy_s[src[1].x](v, (word) src, p0, t0);
  struct loop *t = mo_tag(src);
  thread ini = t->head, d = bump(v, t->end - ini), dst = d;
  for (verb s = ini; (d->x = s->x); s++->x = (word) d++);
  d[1].ap = (vm*) dst;
  return (word) (src - ini + dst); }
