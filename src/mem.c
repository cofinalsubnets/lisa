#include "lips.h"
#include "mem.h"
#include "terp.h"
#define Gc(n) obj n(lips v, obj x, u64 len0, mem base0)
#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((mem)(o),v->pool,v->pool+v->len)
#define COPY(dst,src) (dst=cp(v,src,len0,base0))
#define CP(x) COPY(x,x)

typedef obj copier(lips, obj, u64, mem);
static Inline copier cp;

// unchecked allocator -- make sure there's enough memory!
static u0* bump(lips v, u64 n) {
  u0* x = v->hp;
  return v->hp += n, x; }

// general purpose memory allocator
u0* cells(lips v, u64 n) {
  return Avail >= n || please(v, n) ? bump(v, n) : NULL; }

#include <stdlib.h>
#include <time.h>
// a simple copying garbage collector

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
static clock_t copy(lips v, u64 len1) {
  mem base1;
  bind(base1, malloc(w2b(len1)));
  clock_t t0 = v->t0, t1 = clock();
  u64 len0 = v->len;
  mem base0 = v->pool, sp0 = v->sp, top0 = base0 + len0;

  v->len = len1;
  v->pool = v->hp = base1;
  v->syms = nil;

  i64 delta_r = base1 + len1 - top0;
  v->sp += delta_r, v->fp += delta_r;

  CP(v->ip), CP(v->xp);
  for (int i = 0; i < NGlobs; i++) CP(v->glob[i]);
  while (top0-- > sp0) COPY(v->sp[top0 - sp0], *top0);
  for (root r = v->root; r; r = r->next) CP(*(r->one));

  free(base0);

  clock_t t2 = clock();
  v->t0 = t2;
  return t1 == t2 ? 1 : (t2 - t0) / (t2 - t1); }

// please : li x i64 -> u1
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
u1 please(lips v, u64 req) {
  i64 len = v->len, vit;
  bind(vit, copy(v, len));
  i64 allocd = len - (Avail - req);
  if (growp) do len <<= 1, vit <<= 1; while (growp);
  else if (shrinkp) do len >>= 1, vit >>= 1; while (shrinkp);
  else return true; // no size change needed
  return copy(v, len) || allocd <= v->len; }

#include "hom.h"

static copier cphom, cpvec, cptwo, cpsym, cpstr, cptbl;
static Gc(cpid) { return x; }
// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
static copier *copiers[] = {
  [Hom] = cphom,
  [Num] = cpid,
  [Two] = cptwo,
  [Vec] = cpvec,
  [Str] = cpstr,
  [Tbl] = cptbl,
  [Sym] = cpsym,
  [Nil] = cpid, };

static Inline Gc(cp) {
  return copiers[kind(x)](v, x, len0, base0); }

#define stale(o) inb((mem)(o),base0,base0+len0)
Gc(cphom) {
  hom src = H(x);
  if (fresh(*src)) return (obj) *src;
  hom end = button(src), start = (hom) end[1],
      dst = bump(v, end - start + 2), j = dst;
  for (hom k = start; k < end;)
    j[0] = k[0],
    k++[0] = (terp*) _H(j++);
  j[0] = NULL;
  j[1] = (terp*) dst;
  for (obj u; j-- > dst;
    u = (obj) *j,
    *j = (terp*) (!stale(u) ? u : cp(v, u, len0, base0)));
  return _H(dst += src - start); }

#include "str.h"
Gc(cpstr) {
  str dst, src = S(x);
  return src->len == 0 ? *(mem)src->text :
    (dst = bump(v, Width(str) + b2w(src->len)),
     cpy64(dst->text, src->text, b2w(src->len)),
     dst->len = src->len, src->len = 0,
     *(mem) src->text = _S(dst)); }

#include "sym.h"
Gc(cpsym) {
  sym src = getsym(x), dst;
  if (fresh(src->nom)) return src->nom;
  if (src->nom == nil) // anonymous symbol
    cpy64(dst = bump(v, Width(sym)), src, Width(sym));
  else dst = getsym(sskc(v, &v->syms, cp(v, src->nom, len0, base0)));
  return src->nom = putsym(dst); }

#include "tbl.h"
static ent cpent(lips v, ent src, i64 len0, mem base0) {
  bind(src, src);
  ent dst = (ent) bump(v, Width(ent));
  dst->next = cpent(v, src->next, len0, base0);
  COPY(dst->key, src->key);
  COPY(dst->val, src->val);
  return dst; }

Gc(cptbl) {
  tbl src = gettbl(x);
  if (fresh(src->tab)) return (obj) src->tab;
  i64 src_cap = src->cap;
  tbl dst = bump(v, Width(tbl) + (1<<src_cap));
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (ent*) (dst + 1);
  ent *src_tab = src->tab;
  src->tab = (ent*) puttbl(dst);
  for (u64 ii = 1<<src_cap; ii--;)
    dst->tab[ii] = cpent(v, src_tab[ii], len0, base0);
  return puttbl(dst); }

#include "two.h"
Gc(cptwo) {
  obj dst, src = x;
  if (fresh(A(x))) return A(x);
  dst = puttwo(bump(v, Width(two)));
  A(dst) = A(src), A(src) = dst;
  B(dst) = cp(v, B(src), len0, base0);
  A(dst) = cp(v, A(dst), len0, base0);
  return dst; }

#include "vec.h"
Gc(cpvec) {
  vec dst, src = V(x);
  if (fresh(*src->xs)) return *src->xs;
  dst = bump(v, Width(vec) + src->len);
  i64 i, l = dst->len = src->len;
  dst->xs[0] = src->xs[0];
  src->xs[0] = putvec(dst);
  for (CP(dst->xs[0]), i = 1; i < l; ++i)
    COPY(dst->xs[i], src->xs[i]);
  return _V(dst); }
