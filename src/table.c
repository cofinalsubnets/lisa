#include "i.h"

// FIXME :(
static word hash_hash(core f, word h) { return mix; }

struct typ table_typ = {
  .hash = hash_hash,
};

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

word hash(core v, word x) {
  if (nump(x)) return ror(mix * x, sizeof(word) * 2) ;
  if (datp(x)) return gettyp(x)->hash(v, x);
  if (!bounded(v->pool, x, v->pool+v->len)) return mix ^ (mix * x);
  // it's a function, hash by length
  struct tag *t = ttag((thread) x);
  word len = (cell) t - t->head;
  return mix ^ (mix * len); }

