#include "la.h"
#include "vm.h"

// FIXME this is a totally naive, unproven hashing method.
//
// its performance on hash tables and anonymous functions
// is particularly bad: they all go to the same bucket!
//
// strings, symbols, and numbers do better. for pairs it
// depends on what they contain.
//
// copying GC complicates the use of memory addresses for
// hashing mutable data, which is the obvious way to fix
// the bad cases. we would either need to assign each datum
// a unique identifier when it's created & hash using that,
// or use the address but rehash as part of garbage collection.
//
// TODO replace with something better, verify & benchmark

// just a big random number!
const uint64_t mix = 2708237354241864315;

size_t hashb(const char *at, size_t n) {
  size_t h = 1;
  while (n--) h ^= mix * *at++, h *= mix;
  return h; }

size_t hash(la v, ob x) {
  if (nump(x)) return ror(mix * x, 16);
  if (!livep(v, x)) return mix ^ (x * mix);
  if (G(x) == disp) return ((mtbl) GF(x))->hash(v, x);
  return mix ^ hash(v, hnom(v, x)); }
