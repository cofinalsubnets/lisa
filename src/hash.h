#ifndef _la_hash_h
#define _la_hash_h
intptr_t hash(la, ob);

static Inline size_t ror(size_t x, size_t n) {
  return (x<<((8*sizeof(size_t))-n))|(x>>n); }

extern const int64_t mix;

#endif
