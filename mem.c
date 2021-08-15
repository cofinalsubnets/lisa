#include "lips.h"
#include <stdlib.h>
#include <time.h>
static int copy(lips, u64);
static obj cp(lips, obj, u64, mem);
static Inline u0 do_copy(lips, u64, mem, u64, mem);

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
#define grow() (len<<=1,vit<<=1)
#define shrink() (len>>=1,vit>>=1)
#define growp (allocd > len || vit < 32) // lower bound
#define shrinkp (allocd < (len>>1) && vit >= 128) // upper bound
u0 reqsp(lips v, u64 req) {
 i64 len = v->mem_len, vit = copy(v, len);
 if (vit) { // copy succeeded
  i64 allocd = len - (Avail - req);
  if (growp)        do grow();   while (growp);
  else if (shrinkp) do shrink(); while (shrinkp);
  else return; // no size change needed
  // otherwise grow or shrink ; if it fails maybe we can return
  if (copy(v, len) || allocd <= Len) return; }
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
static int copy(lips v, u64 len) {
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
do_copy(lips v, u64 l0, mem b0, u64 l1, mem b1) {
 mem s0 = Sp, t0 = b0 + l0, t1 = b1 + l1;
 i64 ro = t1 - t0;
 v->mem_len = l1;
 v->mem_pool = Hp = b1;
 Sp += ro, Fp += ro;
 Syms = nil;
 while (t0-- > s0) Sp[t0 - s0] = cp(v, *t0, l0, b0);
#define CP(x) x=cp(v,x,l0,b0)
 for (mroot r = Safe; r; r = r->next) CP(*(r->one));
 CP(Ip), CP(Xp), CP(Glob); }
#undef CP

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
typedef obj cp_(lips, obj, u64, mem);
static cp_ cphom, cptup, cptwo, cpsym, cpstr, cptbl;
#define cpcc(n) static obj n(lips v, obj x, u64 ln, mem lp)

cpcc(cp) {
 switch (kind(x)) {
  case Hom: return cphom(v, x, ln, lp);
  case Vec: return cptup(v, x, ln, lp);
  case Str: return cpstr(v, x, ln, lp);
  case Two: return cptwo(v, x, ln, lp);
  case Sym: return cpsym(v, x, ln, lp);
  case Tbl: return cptbl(v, x, ln, lp);
  default:  return x; } }

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((mem)(o),Pool,Pool+Len)
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
 for (i64 i = 1; i < l; ++i)
  dst->xs[i] = cp(v, src->xs[i], ln, lp);
 return puttup(dst); }

cpcc(cpstr) {
 str dst, src = getstr(x);
 return src->len == 0 ? *(mem)src->text :
  (dst = bump(v, Size(str) + b2w(src->len)),
   cpy64(dst->text, src->text, b2w(src->len)),
   dst->len = src->len, src->len = 0,
   *(mem)src->text = putstr(dst)); }

cpcc(cpsym) {
 sym src = getsym(x), dst;
 if (fresh(src->nom)) return src->nom;
 if (nilp(src->nom)) // anonymous symbol
  dst = bump(v, Size(sym)),
  cpy64(dst, src, Size(sym));
 else dst = getsym(sskc(v, &Syms, cp(v, src->nom, ln, lp)));
 return src->nom = putsym(dst); }

#define stale(o) inb((mem)(o),lp,lp+ln)
cpcc(cphom) {
 hom src = Gh(x);
 if (fresh(G(src))) return (obj) G(src);
 hom end = button(src), start = (hom) G(end+1),
     dst = bump(v, end - start + 2), j = dst;
 for (hom k = start; k < end;
  G(j) = G(k),
  G(k++) = (terp*) Ph(j++));
 G(j) = NULL;
 G(j+1) = (terp*) dst;
 for (obj u; j-- > dst;
  u = (obj) G(j),
  G(j) = (terp*) (stale(u) ? cp(v, u, ln, lp) : u));
 return Ph(dst += src - start); }

static ent cpent(lips v, ent src, i64 ln, mem lp) {
 if (!src) return NULL;
 ent dst = (ent) bump(v, 3);
 dst->next = cpent(v, src->next, ln, lp);
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
 dst->tab = (ent*) (dst + 1);
 ent *src_tab = src->tab;
 src->tab = (ent*) puttbl(dst);
 while (src_cap--)
  dst->tab[src_cap] = cpent(v, src_tab[src_cap], ln, lp);
 return puttbl(dst); }
