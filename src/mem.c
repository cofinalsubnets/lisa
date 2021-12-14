#include "lips.h"
#include "sym.h"
#include "err.h"
#include "two.h"
#include "hom.h"
#include "terp.h"
#include "mem.h"
#include <stdlib.h>
#include <time.h>
static clock_t copy(lips, u64);

// a simple copying garbage collector

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
static clock_t copy(lips v, u64 len1) {

  mem base0 = v->pool, base1 = malloc(w2b(len1));
  u64 len0 = v->len;
  if (!base1) return 0;

  clock_t t0 = v->t0,
          t1 = clock();
  mem sp0 = v->sp,
      top0 = base0 + len0;
  i64 delta_top = base1 + len1 - top0;

  v->len = len1;
  v->pool = v->hp = base1;
  v->sp += delta_top;
  v->fp += delta_top;
  v->syms = nil;

  for (CP(v->ip), CP(v->xp); top0-- > sp0; COPY(Sp[top0 - sp0], *top0));
  for (root r = v->root; r; r = r->next) CP(*(r->one));
  for (int i = 0; i < NGlobs; i++) CP(v->glob[i]);
  free(base0);

  clock_t t2 = clock();
  v->t0 = t2;
  return t1 == t2 ? 1 : (t2 - t0) / (t2 - t1); }

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
GC(cp) {
  switch (kind(x)) {
    case Hom: return cphom(v, x, len0, base0);
    case Vec: return cptup(v, x, len0, base0);
    case Str: return cpstr(v, x, len0, base0);
    case Two: return cptwo(v, x, len0, base0);
    case Sym: return cpsym(v, x, len0, base0);
    case Tbl: return cptbl(v, x, len0, base0);
    default:  return x; } }