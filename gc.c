#include "lips.h"
// a simple garbage collector of the copying type.
//
// normally it maintains one pool of memory. allocations
// increment a pointer into the pool. when the pool isn't
// large enough to support a requested allocation, the
// garbage collector migrates all reachable data into a new
// pool. possible improvements:
// - configurable growth function and time/space bounds
// - semispaces
// - generations
// the first two are fairly simple. generational garbage
// collection would be significantly more complicated because
// it would mean having a write barrier, but it would let us
// avoid a lot of copying. this will probably become more
// important as the amount of persistent data held in memory
// grows: with no generations, this data will be copied often,
// causing the memory manager to allocate more memory to offset
// the extra gc time. but with one or two generations persistent
// data will rarely be copied, which amounts to more efficient
// storage over time.
static int copy(vm, num);
static obj cp(vm, obj, num, mem);

// gc entry point reqsp : vm x num -> bool
// try to return with at least req words of
// available memory. return true on success,
// false otherwise. this function also manages
// the size of the memory pool to keep time and
// space overhead low.
// - copy into a new pool of the same size.
// - if there's enough space and time return success.
// - adjust the size copy again.
// if the first copy fails the request fails.
// if the second copy fails the request succeeds if
// there's enough space (time is ignored).
#define grow() (len*=2,vit*=2)
#define shrink() (len/=2,vit/=2)
#define growp (allocd > len || vit < 32)
#define shrinkp (allocd < len/2 && vit >= 64)
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
static Inline void do_copy(vm, num, mem, num, mem);
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
  for (root r = Safe; r; r = r->next)
    CP(*(r->one)); }
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
  sym dst, src = getsym(x);
  return fresh(src->nom) ? (obj) src->nom :
    (dst = bump(v, Size(sym)),
     dst->nom = cp(v, src->nom, ln, lp),
     src->nom = putsym(dst),
     dst->code = src->code,
     dst->next = Syms,
     Syms = putsym(dst)); }

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
