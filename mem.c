#include "lips.h"
// a simple copying garbage collector
static int copy(vm, num);
static obj cp(vm, obj, num, mem);
static Inline void do_copy(vm, num, mem, num, mem);

// gc entry point reqsp : vm x num -> bool
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
// within certain limits, given here in units of program time
// per unit garbage collection time:
#define lb 32
#define ub 128
// in theory these bounds control the overall time spent in
// garbage collection (3-6% at nonzero allocation). actual
// results may vary, but in an ideal (high-memory) environment
// anything we do to make gc "faster" (other than adjusting the
// limits) will have the actual effect of improving memory
// efficiency.
#define grow() (len*=2,vit*=2)
#define shrink() (len/=2,vit/=2)
#define growp (allocd > len || vit < lb)
#define shrinkp (allocd < len/2 && vit >= ub)
void reqsp(vm v, num req) {
  num len = v->mem_len, vit = copy(v, len);
  if (vit) {
    num allocd = len - (Avail - req);
    if (growp) do grow(); while (growp);
    else if (shrinkp) do shrink(); while (shrinkp);
    else return; // otherwise either grow or shrink
    if (copy(v, len) || allocd <= Len) return; }
  err(v, "gc", 0, "oom"); }

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
// u will always be >= 1. somehow t1 is sometimes
// equal to t2, so in that case u = 1.
static int copy(vm v, num len) {
  clock_t t1 = clock(), t2, u;
  mem b0 = v->mem_pool, b1 = malloc(w2b(len));
  return !b1 ? 0 :
   (do_copy(v, v->mem_len, b0, len, b1),
    free(b0),
    t2 = clock(),
    u = t1 == t2 ? 1 : (t2 - v->t0) / (t2 - t1),
    v->t0 = t2,
    u); }

static Inline void do_copy(vm v, num l0, mem b0, num l1, mem b1) {
  v->mem_len = l1;
  v->mem_pool = Hp = b1;
  mem s0 = Sp,
      t0 = b0 + l0,
      t1 = b1 + l1;
  num ro = t1 - t0;
  Sp += ro, Fp += ro;
  Syms = nil;
  while (t0-- > s0) Sp[t0 - s0] = cp(v, *t0, l0, b0);
#define CP(x) x=cp(v,x,l0,b0)
  CP(Ip), CP(Xp), CP(Syn), CP(v->dict); CP(v->cdict);
  for (root r = Safe; r; r = r->next) CP(*(r->one)); }
#undef CP

// the exact method for copying an object into
// the new pool depends on its type. copied
// objects are used to store pointers to their
// new locations, which effectively destroys the
// old data.
typedef obj cp_(vm, obj, num, mem);
static cp_ cphom, cptup, cptwo, cpsym, cpoct, cptbl;
#define cpcc(n) static obj n(vm v, obj x, num ln, mem lp)

cpcc(cp) {  switch (kind(x)) {
  case Hom: return cphom(v, x, ln, lp);
  case Tup: return cptup(v, x, ln, lp);
  case Oct: return cpoct(v, x, ln, lp);
  case Two: return cptwo(v, x, ln, lp);
  case Sym: return cpsym(v, x, ln, lp);
  case Tbl: return cptbl(v, x, ln, lp);
  default:  return x; } }

#define inb(o,l,u) (o>=l&&o<u)
#define fresh(o) inb((mem)(o),Pool,Pool+Len)
cpcc(cptwo) {
  two dst, src = gettwo(x);
  return fresh(src->x) ? src->x :
    (dst = bump(v, Size(two)),
     dst->x = src->x, src->x = (obj) puttwo(dst),
     dst->y = cp(v, src->y, ln, lp),
     dst->x = cp(v, dst->x, ln, lp),
     puttwo(dst)); }

cpcc(cptup) {
  tup dst, src = gettup(x);
  if (fresh(*src->xs)) return *src->xs;
  dst = bump(v, Size(tup) + src->len);
  num l = dst->len = src->len;
  dst->xs[0] = src->xs[0];
  src->xs[0] = puttup(dst);
  dst->xs[0] = cp(v, dst->xs[0], ln, lp);
  for (num i = 1; i < l; ++i)
    dst->xs[i] = cp(v, src->xs[i], ln, lp);
  return puttup(dst); }

cpcc(cpoct) {
  oct dst, src = getoct(x);
  return src->len == 0 ? *(mem)src->text :
    (dst = bump(v, Size(oct) + b2w(src->len)),
     memcpy(dst->text, src->text, dst->len = src->len),
     src->len = 0,
     *(mem)src->text = putoct(dst)); }

cpcc(cpsym) {
  sym src = getsym(x);
  return fresh(src->nom) ? (obj) src->nom :
    sseekc(v, &Syms, cp(v, src->nom, ln, lp)); }

#define stale(o) inb((mem)(o),lp,lp+ln)
cpcc(cphom) {
  hom dst, src = gethom(x), end = src, start;
  if (fresh(G(src))) return (obj) G(src);
  while (end++->g);
  start = (hom) G(end);
  num i, len = (end+1) - start;
  dst = bump(v, len);
  G(dst+len-2) = NULL;
  G(dst+len-1) = (terp*) dst;
  for (i = 0; i < len - 2; i++)
    G(dst+i) = G(start+i),
    G(start+i) = (terp*) puthom(dst+i);
  for (obj u; i--;)
    u = (obj) G(dst+i),
    G(dst+i) = (terp*) (stale(u) ? cp(v, u, ln, lp) : u);
  return puthom(dst += src - start); }

static tble cptble(vm v, tble src, num ln, mem lp) {
  if (!src) return NULL;
  tble dst = (tble) bump(v, 3);
  dst->next = cptble(v, src->next, ln, lp);
  dst->key = cp(v, src->key, ln, lp);
  dst->val = cp(v, src->val, ln, lp);
  return dst; }

cpcc(cptbl) {
  tbl src = gettbl(x);
  if (fresh(src->tab)) return (obj) src->tab;
  num src_cap = src->cap;
  tbl dst = bump(v, 3 + src_cap);
  dst->len = src->len;
  dst->cap = src_cap;
  dst->tab = (tble*) (dst + 1);
  tble *src_tab = src->tab;
  src->tab = (tble*) puttbl(dst);
  while (src_cap--)
    dst->tab[src_cap] = cptble(v, src_tab[src_cap], ln, lp);
  return puttbl(dst); }


// XXX data constructors

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

// symbols are interned into a binary search tree. we make no
// attempt to keep it balanced but it gets rebuilt in somewhat
// unpredictable order every gc cycle so hopefully that should
// help keep it from getting too bad. a hash table is probably
// the way to go but rebuilding that is more difficult; the
// existing code is unsuitable because it dynamically resizes
// the table and unpredictable memory allocation isn't safe
// during garbage collection. 
static obj sseek(vm v, obj y, obj x) {
  int i = strcmp(symnom(y), chars(x));
  if (i == 0) return y;
  return sseekc(v, i<0?&(getsym(y)->r):&(getsym(y)->l), x); }

obj sseekc(vm v, mem y, obj x) {
  if (!nilp(*y)) return sseek(v, *y, x);
  sym u = bump(v, Size(sym));
  u->nom = x, u->code = hc(x);
  u->l = nil, u->r = nil;
  return *y = putsym(u); }

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

// the least significant bits of the product have the least
// "diffusion", which means their values are determined by the
// fewest number of different bits in the hash code and multiplier.
// they are effectively the bits with the least entropy, but they
// are also the most significant bits to the modulo operation.
// dropping a number of least-significant-bits proportional to the
// length of the table improves key distribution.
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
