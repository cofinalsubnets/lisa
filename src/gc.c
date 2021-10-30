#include "lips.h"
#include "sym.h"
#include "err.h"
#include <stdlib.h>
#include <time.h>
static clock_t copy(lips, u64);
static obj cp(lips, obj, u64, mem);

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
// time in garbage collection should be less than about 6%, at
// the cost of more memory use under pressure.
#define growp (allocd > len || vit < 32) // lower bound
#define shrinkp (allocd < (len>>1) && vit >= 128) // upper bound
u0 reqsp(lips v, u64 req) {
 i64 len = v->len, vit = copy(v, len);
 if (vit) { // copy succeeded
  i64 allocd = len - (Avail - req);
  if (growp) do len <<= 1, vit <<= 1; while (growp);
  else if (shrinkp) do len >>= 1, vit >>= 1; while (shrinkp);
  else return; // no size change needed
  // otherwise grow or shrink ; if it fails maybe we can return
  if (copy(v, len) || allocd <= v->len) return; }
 err(v, "oom"); } // this is a bad error
#undef growp
#undef shrinkp

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
static clock_t copy(lips v, u64 l1) {
 mem b0 = v->pool, b1 = malloc(w2b(l1));
 u64 l0 = v->len;
 if (!b1) return 0;
 clock_t t0 = v->t0, t1 = clock(), t2;
 mem s0 = Sp, tp0 = b0 + l0, tp1 = b1 + l1;
 i64 ro = tp1 - tp0;
 v->len = l1;
 v->pool = Hp = b1;
 Sp += ro, Fp += ro;
 v->syms = nil;
#define COPY(dst,src) (dst=cp(v,src,l0,b0))
#define CP(x) COPY(x,x)
 while (tp0-- > s0)
   COPY(Sp[tp0 - s0], *tp0);
 for (root r = v->root; r; r = r->next)
   CP(*(r->one));
 for (int i = 0; i < NGlobs; i++)
   CP(v->glob[i]);
 CP(v->ip), CP(v->xp);
 free(b0);
 t1 = t1 == (t2 = clock()) ? 1 : (t2 - t0) / (t2 - t1);
 return v->t0 = t2, t1; }


// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
typedef obj copier(lips, obj, u64, mem);
static copier cphom, cptup, cptwo, cpsym, cpstr, cptbl;
#define GC(n) static obj n(lips v, obj x, u64 l0, mem b0)
#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((mem)(o),v->pool,v->pool+v->len)
#define stale(o) inb((mem)(o),b0,b0+l0)

GC(cp) {
 switch (kind(x)) {
  case Hom: return cphom(v, x, l0, b0);
  case Vec: return cptup(v, x, l0, b0);
  case Str: return cpstr(v, x, l0, b0);
  case Two: return cptwo(v, x, l0, b0);
  case Sym: return cpsym(v, x, l0, b0);
  case Tbl: return cptbl(v, x, l0, b0);
  default:  return x; } }

GC(cptwo) {
 obj dst, src = x;
 return fresh(A(x)) ? A(x) :
  (dst = puttwo(bump(v, Size(two))),
   A(dst) = A(src),
   A(src) = dst,
   B(dst) = cp(v, B(src), l0, b0),
   CP(A(dst)),
   dst); }

GC(cptup) {
 vec dst, src = V(x);
 if (fresh(*src->xs)) return *src->xs;
 dst = bump(v, Size(tup) + src->len);
 i64 i, l = dst->len = src->len;
 dst->xs[0] = src->xs[0];
 src->xs[0] = putvec(dst);
 for (CP(dst->xs[0]), i = 1; i < l; ++i)
  COPY(dst->xs[i], src->xs[i]);
 return _V(dst); }

GC(cpstr) {
 str dst, src = S(x);
 return src->len == 0 ? *(mem)src->text :
  (dst = bump(v, Size(str) + b2w(src->len)),
   cpy64(dst->text, src->text, b2w(src->len)),
   dst->len = src->len, src->len = 0,
   *(mem) src->text = _S(dst)); }

GC(cpsym) {
 sym src = getsym(x), dst;
 if (fresh(src->nom)) return src->nom;
 if (src->nom == nil) // anonymous symbol
  cpy64(dst = bump(v, Size(sym)), src, Size(sym));
 else dst = getsym(sskc(v, &v->syms, cp(v, src->nom, l0, b0)));
 return src->nom = putsym(dst); }

GC(cphom) {
 hom src = gethom(x);
 if (fresh(G(src))) return (obj) G(src);
 hom end = button(src), start = (hom) G(end+1),
     dst = bump(v, end - start + 2), j = dst;
 for (hom k = start; k < end;
  G(j) = G(k),
  G(k++) = (terp*) puthom(j++));
 G(j) = NULL;
 G(j+1) = (terp*) dst;
 for (obj u; j-- > dst;
  u = (obj) G(j),
  G(j) = (terp*) (!stale(u) ? u : cp(v, u, l0, b0)));
 return puthom(dst += src - start); }

static ent cpent(lips v, ent src, i64 l0, mem b0) {
 if (!src) return NULL;
 ent dst = (ent) bump(v, 3);
 dst->next = cpent(v, src->next, l0, b0);
 COPY(dst->key, src->key);
 COPY(dst->val, src->val);
 return dst; }

GC(cptbl) {
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
  dst->tab[src_cap] = cpent(v, src_tab[src_cap], l0, b0);
 return puttbl(dst); }
