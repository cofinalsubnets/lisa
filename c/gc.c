#include "la.h"
#include <stdlib.h>
#include <time.h>

// FIXME
// the garbage collector uses stack recursion so a process
// that constructs infinite data will stack overflow, rather
// than fail gracefully with oom. we could fix this by using
// cheney's algorithm but to do that we need to stop using
// tagged pointers.

#define Gc(n) ob n(la v, ob x, intptr_t len0, ob *pool0)
typedef Gc(copier);
static copier
  cphom, cptwo, cpsym, cpstr, cptbl, cpid,
  *copiers[] = {
    [Hom] = cphom, [Num] = cpid, [Two] = cptwo,
    [Str] = cpstr, [Tbl] = cptbl, [Sym] = cpsym, };
static Inline Gc(cp) { return copiers[Q(x)](v, x, len0, pool0); }
static Gc(cpid) { return x; }

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(x) inb(ptr(x),v->pool,v->pool+v->len)

// we use this to test if an object has been moved already
#define COPY(dst,src) (dst=cp(v,src,len0,pool0))
#define CP(x) COPY(x,x)

static Inline ob evacd(la v, ob _, enum class q) {
  ob x = *ptr(_ - q);
  return Q(x) == q && fresh(x) ? x : 0; }

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
static clock_t copy(la v, intptr_t len1) {
  clock_t t0, t1 = clock(), t2;
  ob len0 = v->len,
     *sp0 = v->sp,
     *pool0 = v->pool,
     *pool1 = malloc(len1 * sizeof(ob)),
     *top0 = pool0 + len0,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // fail if we can't get a new pool
  if (!pool1) return 0;

  // copy memory
  v->syms = nil;
  v->len = len1;
  v->pool = v->hp = pool1;
  v->sp = sp0 + shift;
  v->fp = (fr) ((ob*) v->fp + shift);
  CP(v->xp), CP(v->sns), CP(v->wns);
  v->ip = (mo) cp(v, (ob) v->ip, len0, pool0);
  for (int i = 0; i < LexN; CP(v->lex[i]), i++);
  for (ob *sp1 = v->sp; sp0 < top0; COPY(*sp1++, *sp0++));
  for (keep r = v->keep; r; CP(*r->it), r = r->et);

  return
    free(pool0),
    t0 = v->t0,
    v->t0 = t2 = clock(),
    t1 = t2 - t1,
    t1 ? (t2 - t0) / t1 : 1; }

// please : u1 em intptr_t
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

static ob gro(ob allocd, ob len, ob vit) {
  return len <<= 1, vit <<= 1, !growp ? len :
    gro(allocd, len, vit); }

static ob shr(ob allocd, ob len, ob vit) {
  return len >>= 1, vit >>= 1, !shrinkp ? len :
    shr(allocd, len, vit); }

bool please(la v, uintptr_t req) {
  intptr_t len = v->len,
           vit = copy(v, len),
           allocd = len - (Avail - req);
  return len = v->len, !vit ? 0 : !growp && !shrinkp ? 1 :
    copy(v, (growp ? gro : shr)(allocd, len, vit)) ||
    allocd <= v->len; }


#define stale(o) inb((ob*)(o),pool0,pool0+len0)
Gc(cphom) {
  ob e = evacd(v, x, Hom);
  if (e) return e;
  mo src = (mo) x,
     end = button(src),
     start = (mo) end[1].ll,
     dst = bump(v, end - start + 2),
     j = dst;

  for (mo k = start; k < end;)
    j->ll = k->ll,
    k++->ll = (vm*) j++;
  j[0].ll = NULL, j[1].ll = (vm*) dst;

  for (ob u; j-- > dst;)
    u = (ob) j->ll,
    u = !stale(u) ? u : cp(v, u, len0, pool0),
    j->ll = (vm*) u;

  return (ob) (src - start + dst); }

Gc(cpstr) {
  ob e = evacd(v, x, Str);
  if (e) return e;
  str dst, src = (str) (x - Str);
  dst = bump(v, Width(str) + b2w(src->len));
  cpyw(dst->text, src->text, b2w(src->len));
  dst->len = src->len;
  dst->ext = src->ext;
  x = (ob) dst + Str;
  src->ext = x;
  return x; }

Gc(cpsym) {
  ob e = evacd(v, x, Sym);
  if (e) return e;
  sym src = getsym(x), dst;
  return
    src->nom == nil ? // anonymous symbol
      (dst = bump(v, Width(sym)),
       cpyw(dst, src, Width(sym)),
       src->nom = putsym(dst)) :
      (x = cp(v, src->nom, len0, pool0),
       dst = getsym(sskc(v, &v->syms, x)),
       src->nom = putsym(dst) ); }

Gc(cptbl) {
  ob e = evacd(v, x, Tbl);
  if (e) return e;
  tbl src = gettbl(x);
  intptr_t src_cap = src->cap;
  tbl dst = bump(v, Width(tbl));
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (ob*) (dst + 1);
  ob *src_tab = src->tab;
  src->tab = (ob*) puttbl(dst);
  dst->tab = (ob*) cp(v, (ob) src_tab, len0, pool0);
  return puttbl(dst); }

Gc(cptwo) {
  ob dst = evacd(v, x, Two);
  if (!dst)
    dst = put2(bump(v, Width(two))),
    A(dst) = A(x), A(x) = dst,
    B(dst) = cp(v, B(x), len0, pool0),
    A(dst) = cp(v, A(dst), len0, pool0);
  return dst; }

#include "vm.h"
// Run a GC cycle from inside the VM
NoInline Vm(gc) {
  size_t req = v->xp;
  Pack();
  req = please(v, req);
  Unpack();
  return req ? ApY(ip, xp) : ApC(oom_err, xp); }
