#include "lips.h"

static const uint64_t mix = 2708237354241864315;
static uint64_t hc(obj);
// internal memory allocator
Inline void *bump(vm v, num n) {
  void *x = v->hp; v->hp += n; return x; }

// the public interface (used in other files)
void *cells(vm v, num n) {
  if (Avail < n) reqsp(v, n);
  return bump(v, n); }

////
/// data constructors and utility functions
//

static NoInline obj pair_gc(vm v, obj a, obj b) {
  with(a, with(b, reqsp(v, 2)));
  two t = bump(v, 2);
  return t->x = a, t->y = b, puttwo(t); }

// pairs
obj pair(vm v, obj a, obj b) {
  if (Avail < 2) return pair_gc(v, a, b);
  two t = bump(v, 2);
  return t->x = a, t->y = b, puttwo(t); }

// strings
obj string(vm v, const char *c) {
  num bs = 1 + strlen(c);
  oct o = cells(v, Size(oct) + b2w(bs));
  memcpy(o->text, c, o->len = bs);
  return putoct(o); }

//symbols
obj interns(vm v, const char *c) {
  return intern(v, string(v, c)); }

obj sseekc(vm, mem, obj), seekc(vm, obj, obj);
static obj sseek(vm v, obj y, obj x) {
  int i = strcmp(symnom(y), chars(x));
  if (i == 0) return y;
  return sseekc(v, i<0?&(getsym(y)->r):&(getsym(y)->l), x); }
obj sseekc(vm v, mem y, obj x) {
  if (nilp(*y)) {
    sym u = bump(v, Size(sym));
    u->nom = x, u->code = hc(x);
    u->l = nil, u->r = nil;
    return *y = putsym(u); }
  return sseek(v, *y, x); }

obj intern(vm v, obj x) {
  if (Avail < Size(sym)) with(x, reqsp(v, Size(sym)));
  return sseekc(v, &Syms, x); }

static Inline uint64_t hash_bytes(num len, char *us) {
  num h = 1;
  for (; len--; h *= mix, h ^= *us++);
  return mix * h; }

static uint64_t hc(obj x) {
  switch (kind(x)) {
    case Sym: return getsym(x)->code;
    case Oct: return hash_bytes(getoct(x)->len, getoct(x)->text);
    case Two: return hc(X(x)) ^ hc(Y(x));
    default:  return mix * x; } }

// the least significant bits of the product have the least "diffusion",
// which means their values are determined by the fewest number of different
// bits in the hash code and multiplier. they are effectively the bits with
// the least entropy, but they are also the most significant bits to
// the modulo operation. dropping a number of least-significant-bits
// proportional to the length of the table improves key distribution.
static Inline uint64_t hb_idx(num cap, uint64_t code) {
  return (code / (cap<<10)) % cap; }

static Inline tble hb(obj t, uint64_t code) {
  return gettbl(t)->tab[hb_idx(gettbl(t)->cap, code)]; }

// tbl_resize(vm, tbl, new_size): destructively resize a hash table.
// new_size words of memory are allocated for the new bucket array.
// the old table entries are reused to populate the modified table.
static void tbl_resize(vm v, obj t, num ns) {
  tble e, ch, *b, *d;
  with(t, b = memset(cells(v, ns), 0, w2b(ns)));
  tbl o = gettbl(t);
  num u, n = o->cap;
  d = o->tab; o->tab = b; o->cap = ns;
  while (n--) for (ch = d[n]; ch;)
    e = ch,
    ch = ch->next,
    u = hb_idx(ns, hc(e->key)),
    e->next = b[u],
    b[u] = e; }

static num tbl_k_elen(tble e, num i) {
  return e ? tbl_k_elen(e->next, i+1) : i; }
// simple hashing performance metric. will be > 1 if keys are
// sufficiently unevenly distributed.
static num tbl_k(obj t) {
  tbl o = gettbl(t);
  num i = 0, j = 0, k;
  for (; j < o->cap; j++)
    k = tbl_k_elen(o->tab[j], 0),
    i += k * k;
  return i / o->cap; }

static void tbl_add_entry(vm v, obj t, obj k, obj x, num b) {
  tble e;
  with(t, with(k, with(x, e = cells(v, Size(tble)))));
  tbl y = gettbl(t);
  e->key = k, e->val = x;
  e->next = y->tab[b], y->tab[b] = e;
  ++y->len; }

obj tbl_set(vm v, obj t, obj k, obj val) {
  num b = hb_idx(gettbl(t)->cap, hc(k));
  tble e = gettbl(t)->tab[b];
  for (;e; e = e->next)
    if (e->key == k) return e->val = val;
  mm(&t); mm(&val);
  tbl_add_entry(v, t, k, val, b);
  if (tbl_k(t) > 2) tbl_resize(v, t, gettbl(t)->cap * 2) ;
  return um, um, val; }

static obj tbl_keys_j(vm v, tble e, obj l) {
  if (!e) return l;
  obj x = e->key;
  with(x, l = tbl_keys_j(v, e->next, l));
  return pair(v, x, l); }

static obj tbl_keys_i(vm v, obj t, num i) {
  if (i == gettbl(t)->cap) return nil;
  obj k;
  with(t, k = tbl_keys_i(v, t, i+1));
  return tbl_keys_j(v, gettbl(t)->tab[i], k); }

obj tbl_keys(vm v, obj t) {
  return tbl_keys_i(v, t, 0); }

obj tbl_del(vm v, obj t, obj k) {
  tbl y = gettbl(t);
  obj r = nil;
  num b = hb_idx(y->cap, hc(k));
  tble e = y->tab[b];
  struct tble _v = {0,0,e};
  for (tble l = &_v; l && l->next; l = l->next)
    if (l->next->key == k) {
      r = l->next->val;
      l->next = l->next->next;
      y->len--;
      break; }
  y->tab[b] = _v.next;
  if (y->len && y->cap / y->len > 2)
    with(r, with(t, tbl_resize(v, t, y->cap / 2)));
  return r; }

obj tbl_get(vm v, obj t, obj k) {
  tble e = hb(t, hc(k));
  for (;e; e = e->next) if (eql(e->key, k)) return e->val;
  return 0; }

obj table(vm v) {
  tbl t = cells(v, sizeof(struct tbl)/w2b(1) + 1);
  tble *b = (tble*)(t+1);
  *b = NULL;
  t->tab = b;
  t->len = 0;
  t->cap = 1;
  return puttbl(t); }
