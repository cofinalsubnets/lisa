#include "lips.h"
static int copy(V, Z);
static obj cp(lips, obj, i64, mem);
static Inline u0 do_copy(lips, i64, mem, i64, mem);

// a simple copying garbage collector

// gc entry point reqsp : vm x num -> bool
//
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// manages the size of the memory pool. here is the procedure
// in a nutshell:
//
// - copy into a new pool of the same size. if this fails,
//   the request fails (oom).
// - if there's enough space and the garbage collector
//   is running fast enough, return success.
// - otherwise adjust the size and copy again. if this fails,
//   we can still succeed if the first copy left us with
//   enough free space (ie. we tried to grow for performance
//   reasons). but otherwise the request fails (oom).
//
// at a constant rate of allocation, doubling the size of the
// heap halves the amount of time spent in garbage collection.
// the memory manager uses this relation to automatically trade
// space for time to keep the time spent in garbage collection
// under a certain proportion of total running time: amortized
// time in garbage collection should e than about 6%, at the cost of
// less efficient memory use under pressure.
#define grow() (len*=2,vit*=2)
#define shrink() (len/=2,vit/=2)
#define growp (allocd > len || vit < 32) // lower bound
#define shrinkp (allocd < len/2 && vit >= 128) // upper bound
u0 reqsp(lips v, i64 req) {
 i64 len = v->mem_len, vit = copy(v, len);
 if (vit) { // copy succeeded
  i64 allocd = len - (Avail - req);
  if (growp)        do grow();   while (growp);
  else if (shrinkp) do shrink(); while (shrinkp);
  else return; // no size change needed
  // otherwise grow or shrink
  if (copy(v, len)) return;
  // oh no that didn't work
  // maybe we can still return though
  if (allocd <= Len) return; } // aww darn
 errp(v, "oom"); // this is a bad error
 restart(v); }

// the first step in copying is to allocate
// a new pool of the given length, which must
// be at least enough to support the actual
// amount of reachable memory. if this fails
// then return 0. otherwise swap the pools,
// reset internal symbols, copy the stack,
// global variables, and any user saved
// locations, and free the old pool. then
// return u:
//
//     non-gc running time     t1    t2
// ,.........................,/      |
// -----------------------------------
// |                          `------'
// t0                  gc time (this cycle)
//
//       u = (t2 - t0) / (t2 - t1)
//
// t values come from clock(). if t0 < t1 < t2 then
// u will be >= 1. however, sometimes t1 == t2. in that case
// u = 1.
static int copy(V v, Z len) {
 clock_t t1 = clock(), t2, u;
 mem b0 = v->mem_pool, b1 = malloc(w2b(len));
 return !b1 ? 0 :
  (do_copy(v, v->mem_len, b0, len, b1),
   free(b0),
   t2 = clock(),
   u = t1 == t2 ? 1 : (t2 - v->t0) / (t2 - t1),
   v->t0 = t2,
   u); }

static Inline u0
do_copy(lips v, i64 l0, mem b0, i64 l1, mem b1) {
 M s0 = Sp, t0 = b0 + l0, t1 = b1 + l1;
 Z ro = t1 - t0;
 v->mem_len = l1;
 v->mem_pool = Hp = b1;
 Sp += ro, Fp += ro;
 Syms = nil;
 Wh (t0-- > s0) Sp[t0 - s0] = cp(v, *t0, l0, b0);
#define CP(x) x=cp(v,x,l0,b0)
 CP(Ip), CP(Xp), CP(Glob);
 Fo (Mp r = Safe; r; r = r->next) CP(*(r->one)); }
#undef CP

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
typedef obj cp_(lips, obj, i64, mem);
static cp_ cphom, cptup, cptwo, cpsym, cpoct, cptbl;
#define cpcc(n) static obj n(lips v, obj x, i64 ln, mem lp)

cpcc(cp) {
 switch (kind(x)) {
  case Hom: return cphom(v, x, ln, lp);
  case Tup: return cptup(v, x, ln, lp);
  case Oct: return cpoct(v, x, ln, lp);
  case Two: return cptwo(v, x, ln, lp);
  case Sym: return cpsym(v, x, ln, lp);
  case Tbl: return cptbl(v, x, ln, lp);
  Df:       return x; } }

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((M)(o),Pool,Pool+Len)
cpcc(cptwo) {
 two dst, src = gettwo(x);
 return fresh(src->x) ? src->x :
  (dst = bump(v, Size(two)),
   dst->x = src->x, src->x = (obj) puttwo(dst),
   dst->y = cp(v, src->y, ln, lp),
   dst->x = cp(v, dst->x, ln, lp),
   puttwo(dst)); }

cpcc(cptup) {
 vec dst, src = gettup(x);
 if (fresh(*src->xs)) return *src->xs;
 dst = bump(v, Size(tup) + src->len);
 i64 l = dst->len = src->len;
 dst->xs[0] = src->xs[0];
 src->xs[0] = puttup(dst);
 dst->xs[0] = cp(v, dst->xs[0], ln, lp);
 for (Z i = 1; i < l; ++i)
   dst->xs[i] = cp(v, src->xs[i], ln, lp);
 return puttup(dst); }

cpcc(cpoct) {
 str dst, src = getoct(x);
 return src->len == 0 ? *(M)src->text :
  (dst = bump(v, Size(oct) + b2w(src->len)),
   wcpy(dst->text, src->text, b2w(src->len)),
   dst->len = src->len, src->len = 0,
   *(M)src->text = putoct(dst)); }

cpcc(cpsym) {
 sym src = getsym(x), dst;
 if (fresh(src->nom)) return src->nom;
 if (nilp(src->nom)) // anonymous symbol
  dst = bump(v, Size(sym)),
  wcpy(dst, src, Size(sym));
 else dst = getsym(sskc(v, &Syms, cp(v, src->nom, ln, lp)));
 return src->nom = putsym(dst); }

#define stale(o) inb((M)(o),lp,lp+ln)
cpcc(cphom) {
 hom dst, src = Gh(x), end = src, start;
 if (fresh(G(src))) return (obj) G(src);
 while (*end) end++;
 start = (H) G(end+1);
 i64 len = (end+2) - start;
 dst = bump(v, len);
 hom j = dst;
 for (H k = start; k < end; j++, k++)
  G(j) = G(k), G(k) = (T) Ph(j);
 G(j) = NULL;
 G(j+1) = (T) dst;
 for (obj u; j-- > dst;)
  u = (obj) G(j),
  G(j) = (T) (stale(u) ? cp(v, u, ln, lp) : u);
 return Ph(dst += src - start); }

static tble cptble(lips v, tble src, i64 ln, mem lp) {
 if (!src) return NULL;
 tble dst = (tble) bump(v, 3);
 dst->next = cptble(v, src->next, ln, lp);
 dst->key = cp(v, src->key, ln, lp);
 dst->val = cp(v, src->val, ln, lp);
 return dst; }

cpcc(cptbl) {
 tbl src = gettbl(x);
 if (fresh(src->tab)) return (obj) src->tab;
 i64 src_cap = src->cap;
 tbl dst = bump(v, 3 + src_cap);
 dst->len = src->len;
 dst->cap = src_cap;
 dst->tab = (tble*) (dst + 1);
 tble *src_tab = src->tab;
 src->tab = (tble*) puttbl(dst);
 while (src_cap--)
  dst->tab[src_cap] = cptble(v, src_tab[src_cap], ln, lp);
 return puttbl(dst); }
