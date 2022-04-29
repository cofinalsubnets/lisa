#include "lips.h"
#include "terp.h"
#include <stdlib.h>
#include <time.h>

typedef ob copier(en, ob, u64, ob*);
static Inline copier cp;
#define Gc(n) ob n(en v, ob x, u64 len0, ob*pool0)

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((ob*)(o),v->pool,v->pool+v->len)
#define COPY(dst,src) (dst=cp(v,src,len0,pool0))
#define CP(x) COPY(x,x)

// unchecked allocator -- make sure there's enough memory!
static u0* bump(en v, u64 n) {
  u0* x = v->hp;
  return v->hp += n, x; }

// general purpose memory allocator
u0* cells(en v, u64 n) {
  return Avail >= n || please(v, n) ? bump(v, n) : 0; }

#define oom_err_msg "out of memory : %d + %d"
// Run a GC cycle from inside the VM
Vm(gc) {
  u64 req = v->xp;
  Pack();
  if (!please(v, req)) return err(v, oom_err_msg, v->len, req); 
  Unpack();
  Next(0); }

// a simple copying garbage collector
//
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
static clock_t copy(en v, u64 len1) {
  ob* pool1;
  clock_t t0, t1 = clock(), t2;
  bind(pool1, malloc(w2b(len1)));

  u64 len0 = v->len;
  ob *pool0 = v->pool,
     *sp0 = v->sp,
     *top0 = pool0 + len0;
  i64 shift = pool1 + len1 - top0;

  v->sp = sp0 + shift;
  v->fp = v->fp + shift;
  v->len = len1;
  v->pool = v->hp = pool1;
  v->syms = nil;

  CP(v->ip), CP(v->xp);
  for (int i = 0; i < NGlobs; i++) CP(v->glob[i]);
  for (ob *sp1 = v->sp; sp0 < top0; COPY(*sp1++, *sp0++));
  for (mm r = v->mm; r; r = r->et) CP(*(r->it));

  free(pool0);
  t0 = v->t0;
  v->t0 = t2 = clock();
  t1 = t2 - t1;
  return t1 ? (t2 - t0) / t1 : 1; }

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
u1 please(en v, u64 req) {
  i64 len = v->len, vit;
  bind(vit, copy(v, len));
  i64 allocd = len - (Avail - req);
  if (growp) do len <<= 1, vit <<= 1; while (growp);
  else if (shrinkp) do len >>= 1, vit >>= 1; while (shrinkp);
  else return true; // no size change needed
  return copy(v, len) || allocd <= v->len; }


static copier cphom, cpvec, cptwo, cpsym, cpstr, cptbl;
static Gc(cpid) { return x; }
// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
static copier *copiers[] = {
  [Hom] = cphom, [Num] = cpid, [Two] = cptwo, [Vec] = cpvec,
  [Str] = cpstr, [Tbl] = cptbl, [Sym] = cpsym, [Nil] = cpid, };

static Inline Gc(cp) { return copiers[Q(x)](v, x, len0, pool0); }

#define stale(o) inb((ob*)(o),pool0,pool0+len0)
Gc(cphom) {
  yo src = H(x);
  if (fresh(src->ll)) return (ob) src->ll;
  yo end = button(src), start = (yo) end[1].ll,
     dst = bump(v, end - start + 2), j = dst;
  for (yo k = start; k < end;)
    j->ll = k->ll,
    k++->ll = (vm*) (ob) (j++);
  j[0].ll = NULL;
  j[1].ll = (vm*) dst;
  for (ob u; j-- > dst;
    u = (ob) j->ll,
    j->ll = (vm*) (!stale(u) ? u : cp(v, u, len0, pool0)));
  return (ob) (dst += src - start); }

Gc(cpstr) {
  str dst, src = S(x);
  return src->len == 0 ? *(ob*)src->text :
    (dst = bump(v, Width(str) + b2w(src->len)),
     cpy64(dst->text, src->text, b2w(src->len)),
     dst->len = src->len, src->len = 0,
     *(ob*) src->text = _S(dst)); }

Gc(cpsym) {
  sym src = getsym(x), dst;
  if (fresh(src->nom)) return src->nom;
  if (src->nom == nil) // anonymous symbol
    cpy64(dst = bump(v, Width(sym)), src, Width(sym));
  else dst = getsym(sskc(v, &v->syms, cp(v, src->nom, len0, pool0)));
  return src->nom = putsym(dst); }

static ent cpent(en v, ent src, i64 len0, ob *pool0) {
  bind(src, src);
  ent dst = (ent) bump(v, Width(ent));
  dst->next = cpent(v, src->next, len0, pool0);
  COPY(dst->key, src->key);
  COPY(dst->val, src->val);
  return dst; }

Gc(cptbl) {
  tbl src = gettbl(x);
  if (fresh(src->tab)) return (ob) src->tab;
  i64 src_cap = src->cap;
  tbl dst = bump(v, Width(tbl) + (1<<src_cap));
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (ent*) (dst + 1);
  ent *src_tab = src->tab;
  src->tab = (ent*) puttbl(dst);
  for (u64 ii = 1<<src_cap; ii--;)
    dst->tab[ii] = cpent(v, src_tab[ii], len0, pool0);
  return puttbl(dst); }

Gc(cptwo) {
  ob dst, src = x;
  if (fresh(A(x))) return A(x);
  dst = puttwo(bump(v, Width(two)));
  A(dst) = A(src), A(src) = dst;
  B(dst) = cp(v, B(src), len0, pool0);
  A(dst) = cp(v, A(dst), len0, pool0);
  return dst; }

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
