#include "lips.h"
#include "mem.h"
#include "terp.h"
#include "err.h"
#include "sym.h"
#include "two.h"
#include "hom.h"
#include "vec.h"
#include <stdlib.h>
#include <time.h>

// memory allocation functions

// general purpose memory allocator
u0* cells(lips v, u64 n) {
  if (Avail < n) reqsp(v, n);
  return bump(v, n); }

// unchecked allocator -- make sure there's enough memory!
u0* bump(lips v, u64 n) {
  u0* x = v->hp;
  return v->hp += n, x; }

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
  clock_t t0 = v->t0,
          t1 = clock();

  u64 len0 = v->len;
  mem base0 = v->pool,
      base1 = malloc(w2b(len1)),
      sp0 = v->sp,
      top0 = base0 + len0;

  if (!base1) return 0;


  v->len = len1;
  v->pool = v->hp = base1;
  v->syms = nil;

  i64 delta_r = base1 + len1 - top0;
  v->sp += delta_r;
  v->fp += delta_r;

  CP(v->ip), CP(v->xp);
  for (int i = 0; i < NGlobs; i++) CP(v->glob[i]);
  while (top0-- > sp0) COPY(v->sp[top0 - sp0], *top0);
  for (root r = v->root; r; r = r->next) CP(*(r->one));

  free(base0);

  clock_t t2 = clock();
  v->t0 = t2;
  return t1 == t2 ? 1 : (t2 - t0) / (t2 - t1); }

// gc entry point reqsp : vm x num -> ()
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
  errp(v, "oom");
  restart(v); }
#undef growp
#undef shrinkp

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
GC(cp) {
  switch (kind(x)) {
    default:  return x;
    case Hom: return cphom(v, x, len0, base0);
    case Vec: return cpvec(v, x, len0, base0);
    case Str: return cpstr(v, x, len0, base0);
    case Two: return cptwo(v, x, len0, base0);
    case Sym: return cpsym(v, x, len0, base0);
    case Tbl: return cptbl(v, x, len0, base0); } }
