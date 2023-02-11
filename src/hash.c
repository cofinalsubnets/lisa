#include "i.h"

// this is a totally ad hoc, unproven hashing method.
//
// its performance on hash tables and anonymous functions
// is very bad (they all go to the same bucket!)
//
// strings, symbols, and numbers do better. for pairs it
// depends on what they contain.
//
// copying GC complicates the use of memory addresses for
// hashing mutable data, which is the obvious way to fix
// the bad cases. we would either need to assign each datum
// a unique identifier when it's created & hash using that,
// or use the address but rehash as part of garbage collection.

uintptr_t hash(la v, ob x) {
  if (nump(x)) return ror(mix * x, sizeof(uintptr_t) * 2);
  if (G(x) == act) return gettyp(x)->hash(v, x);
  if (!livep(v, x)) return mix ^ (x * mix);
  return mix ^ hash(v, hnom(v, (mo) x)); }

uintptr_t hx_two(la v, ob x) {
  uintptr_t hc = hash(v, A(x)) * hash(v, B(x));
  return ror(hc, 4 * sizeof(uintptr_t)); }

uintptr_t hx_sym(la v, ob _) {
  return ((sym) _)->code; }

uintptr_t hx_str(la v, ob _) {
  str s = (str) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(ob),
         bytes = s->len % sizeof(ob);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

// hash tables are hashed by their type
uintptr_t hx_typ(la v, ob _) {
  return ror(mix * (uintptr_t) GF(_), 16); }
