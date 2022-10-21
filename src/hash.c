#include "lisa.h"
#include "vm.h"

// FIXME this is a totally naive, unproven hashing method.
// TODO replace with something better, verify & benchmark

// just a big random number!
static const uint64_t mix = 2708237354241864315;

static Inline size_t ror(size_t x, size_t n) {
  return (x<<((8*sizeof(size_t))-n))|(x>>n); }

size_t hashb(const char *at, size_t n) {
  size_t h = 1;
  while (n--) h ^= mix * *at++, h *= mix;
  return h; }

size_t hash(la v, ob x) {
  switch (TypeOf(x)) {
    case Sym: return getsym(x)->code;
    case Two: return ror(hash(v, A(x)) * hash(v, B(x)), 32);
    case Tbl: return ror(mix * Tbl, 48);
    case Num: return ror(mix * x, 16);
    case Str: return hashb(getstr(x)->text, getstr(x)->len); }
  if (!livep(v, x)) return mix ^ (x * mix);
  if (G(x) == disp) return ((mtbl) GF(x))->hash(v, x);
  return mix ^ hash(v, hnom(v, x)); }
