#include "lisa.h"
#include "vm.h"
#include <stdlib.h>
#include <time.h>

static clock_t copy(la, size_t);

// please : u1 la size_t
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
// heap size is governed by a simple feedback mechanism. at a
// constant rate of allocation, doubling the size of the heap
// halves the amount of time spent in garbage collection. the
// memory manager uses this relation to automatically trade
// space for time to keep the time spent in garbage collection
// under a certain proportion of total running time: amortized
// time in garbage collection should be under about 6%, at the
// cost of more memory use under pressure.

bool please(la v, size_t req) {
  size_t len = v->len, vit = copy(v, len);
  if (!vit) return 0;
  size_t tar = len, all = len - (Avail - req);
  while (all > tar || vit < 32) tar <<= 1, vit <<= 1;
  while (all < (tar>>1) && vit >= 128) tar >>= 1, vit >>= 1;
  return tar == len || copy(v, tar) || all <= len; }

// FIXME
//
// the garbage collector works pretty well but it could be better:
//
// - it uses stack recursion so a process that constructs infinite
//   data will stack overflow, rather than fail gracefully with oom.
//   we could fix this with cheney's algorithm  but for that we would
//   need to give up tagged pointers.
//
// - we allocate a new pool every cycle rather than keeping two pools
//   at all times. theoretically this means we have less memory allocated
//   most of the time, and if malloc is efficient then the overhead from
//   calling it every cycle should be negligible, but it would still be
//   better only to call out when we need to grow or shrink the pool.
//
// - it'd be nice to scale by fibonaccis instead of powers of 2

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(x) inb(((ob*) (x)),v->pool,v->pool+v->len)

// we use this to test if an object has been moved already
#define COPY(dst,src) (dst=cp(v,src,len0,pool0))
#define CP(x) COPY(x,x)

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
// t values come from clock(). if t0 < t1 < t2
// then u will be >= 1. however, sometimes
// t1 == t2. in that case u = 1.
static clock_t copy(la v, size_t len1) {
  clock_t t0, t1 = clock(), t2;

  ob *pool1 = calloc(len1, sizeof(ob));
  if (!pool1) return 0;

  ob len0 = v->len,
     *sp0 = v->sp,
     *pool0 = v->pool,
     *top0 = pool0 + len0,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // copy memory
  v->syms = nil;
  v->len = len1;
  v->pool = v->hp = pool1;
  v->sp = sp0 + shift;
  v->fp = (fr) ((ob*) v->fp + shift);
  CP(v->xp), CP(v->topl);
  v->ip = (mo) cp(v, (ob) v->ip, len0, pool0);

  for (size_t i = 0; i < LexN; CP(v->lex[i]), i++);
  for (keep r = v->keep; r; CP(*r->it), r = r->et);

  // copy the stack
  // TODO do this a little more intelligently so we can store
  // bare numbers in frames.
  for (ob *sp1 = v->sp; sp0 < top0; COPY(*sp1++, *sp0++));

  free(pool0);
  t0 = v->t0;
  v->t0 = t2 = clock();
  t1 = t2 - t1;
  return t1 ? (t2 - t0) / t1 : 1; }

Gc(cphom) {
  // this is not a very good way to find the head :(
  // since many function references will be to the head it
  // would be an easy optimization to have a head bracket
  // pointing to the tail.
  mo src = (mo) x,
     end = button(src),
     start = (mo) GF(end),
     dst = bump(v, end - start + 2),
     j = dst;

  for (mo k = start; k < end; G(j) = G(k), G(k++) = (vm*) j++);
  for (G(j) = NULL, GF(j) = (vm*) dst; j-- > dst;
    G(j) = (vm*) cp(v, (ob) G(j), len0, pool0));

  return (ob) (src - start + dst); }

#define stale(o) inb((ob*)(o),pool0,pool0+len0)
Gc(cp) {
  if (nump(x) || !stale(x)) return x;
  ob y = (ob) G(x);
  if (!nump(y) && livep(v, y)) return y;
  if ((vm*) y == disp) return
    ((mtbl) GF(x))->copy(v, x, len0, pool0);
  return cphom(v, x, len0, pool0); }

#include "vm.h"
// Run a GC cycle from inside the VM
// XXX calling convention: size of request (bare size_t) in v->xp
NoInline Vm(gc) {
  size_t req = v->xp;
  CallOut(req = please(v, req));
  return req ? ApY(ip, xp) : ApC(oom_err, xp); }
