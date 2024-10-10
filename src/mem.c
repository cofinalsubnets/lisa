#include "i.h"


NoInline Vm(gc, size_t n) {
  return Pack(f), !f->please(f, n) ? Oom :
    f->ip->ap(f, f->ip, f->hp, f->sp); }
void *bump(core f, size_t n) {
  void *x = f->hp;
  f->hp += n;
  return x; }

void *cells(state f, size_t n) { return
  n <= avail(f) || f->please(f, n) ? bump(f, n) : 0; }

bool static_please(core f, size_t req) {
  word *p0 = f->pool, *p1 = f->loop;
  f->pool = p1, f->loop = p0;
  copy_from(f, p0, f->len);
  return avail(f) >= req; }

// garbage collector
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
bool libc_please(core f, size_t req) {
  word *b0p0 = f->pool, *b0p1 = f->loop;
  f->pool = b0p1, f->loop = b0p0;
  size_t t0 = f->t0, t1 = clock(),
         len0 = f->len;

  copy_from(f, b0p0, len0);

  req += len0 - avail(f);
  size_t t2 = f->t0 = clock(),
         v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1),
         len1 = len0;

  // how to calculate whether v in bounds
#define too_little (len1 < req || v < v_lo)
#define too_big (len1 >> 1 > req && v > v_hi)

  // resize or no ?
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true;

  // too big or too small so try and adjust
  word *b1p0 = malloc(len1 * 2 * sizeof(word));
  if (!b1p0) return req <= len0;

  f->len = len1;
  f->pool = b1p0;
  f->loop = b1p0 + len1;
  copy_from(f, b0p1, len0),

  free(b0p0 < b0p1 ? b0p0 : b0p1),
  f->t0 = clock();
  return true; }

NoInline void copy_from(core f, word *p0, size_t len0) {
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
  f->dict = (table) cp(f, (word) f->dict, p0, t0);
  f->macro = (table) cp(f, (word) f->macro, p0, t0);
  f->symbols = 0;
  // copy stack
  for (size_t i = 0; i < slen; i++) sp1[i] = cp(f, sp0[i], p0, t0);
  // copy managed values
  for (struct mm *r = f->safe; r; r = r->next) *r->addr = cp(f, *r->addr, p0, t0);
  // cheney's algorithm
  for (thread k; (k = (thread) f->cp) < (thread) f->hp;)
    if (datp(k)) k[1].typ->evac(f, (word) k, p0, t0); // is data
    else { for (; k->ap; k++) k->x = cp(f, k->x, p0, t0); // is thread
           f->cp = (word*) k + 2; } }

NoInline word cp(state v, word x, word *p0, word *t0) {
  // if it's a number or out of managed memory then return it
  if (!bounded(p0, x, t0)) return x;
  cell src = (cell) x;
  x = src->x;
  // if the cell holds a pointer to the new space then return the pointer
  if (homp(x) && bounded(v->pool, x, v->pool + v->len)) return x;
  // if it's data then call the given copy function
  if (datp(src)) return src[1].typ->copy(v, (word) src, p0, t0);
  // it's a thread, find the end
  struct tag *t = ttag(src);
  thread ini = t->head, d = bump(v, t->end - ini), dst = d;
  for (cell s = ini; (d->x = s->x); s++->x = (word) d++);
  d[1].ap = (vm*) dst;
  return (word) (src - ini + dst); }
