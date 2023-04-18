#include "i.h"

enum status act(li v) {
  ob t = v->sp[1];
  v->sp[1] = v->sp[0];
  v->sp[0] = t;
  return Ok; }

ob pop1(li v) { return *v->sp++;}
static NoInline ob push1_gc(li v, ob x) {
  bool ok; return with(x, ok = please(v, 1)),
                  ok ? (*--v->sp = x) : 0; }
ob push1(li v, ob x) {
  return Avail ? (*--v->sp = x) : push1_gc(v, x); }

// push things onto the stack
static NoInline bool pushsr(li v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i + 1, xs)),
     _ && (*--v->sp = x, true)); }

NoInline bool pushs(li v, ...) {
  bool _; va_list xs; return
    va_start(xs, v),
    _ = pushsr(v, 0, xs),
    va_end(xs),
    _; }

enum status li_go(li v) {
  return ((enum status (*)(li))*v->sp++)(v); }


void li_fin(li v) { if (v)
  free(v->pool < v->loop ? v->pool : v->loop),
  v->pool = v->loop = NULL; }


// initialize a state
NoInline enum status li_ini(li v) {
  memset(v, 0, sizeof(struct V));
  const size_t len = 1 << 10; // a power of 2
  ob *pool = new_pool(len);
  if (!pool) return OomError;
  v->len = len,
  v->pool = v->hp = pool,
  v->loop = pool + len,
  v->sp = pool + len,
  v->t0 = clock();
  return Ok; }

static bool eq_two(li v, ob x, ob y) { // FIXME can overflow
  return htwop((mo) y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }
static bool eq_str(li v, ob x, ob y) {
  if (!hstrp((mo) y)) return false;
      str a = (str) x, b = (str) y;
      return a->len == b->len && !strncmp(a->text, b->text, a->len); }

static bool (*const data_equi[])(li, ob, ob) = {
 [Two] = eq_two, [Str] = eq_str, };
bool eql(li v, ob a, ob b) { return a == b ||
  (!nump(a|b) && datp((mo) a) &&
   data_equi[gettyp(a)](v, a, b)); }

// rng
intptr_t liprng(intptr_t in) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (steele_vigna_2021 * in + 1) >> 8; }

typedef ob gc_evac(li, ob, ob*, ob*);
static gc_evac cp_str, cp_two,
  *const data_evac[] = {
    [Two] = cp_two,
    [Str] = cp_str, };

typedef void gc_walk(li, ob, ob*, ob*);
static gc_walk wk_str, wk_two,
  *const data_walk[] = {
    [Two] = wk_two,
    [Str] = wk_str, };

ob *new_pool(size_t n) { return malloc(n * 2 * sizeof(ob)); }

////
/// garbage collector
//
// it's a dynamic semispace copying collector that uses
// cheney's algorithm to avoid stack recursion.
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. this function also
// governs the size of the memory pool by attempting to keep
//
//   vim = t1 == t2 ? 1 : (t2 - t0) / (t2 - t1)
//
// between
#define MinVim 8
// and
#define MaxVim (MinVim<<6)
// where
//
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static void copy_from(li, ob*, ob*);
NoInline bool please(li v, size_t req) {
  size_t t1 = clock(), t0 = v->t0, have = v->len;
  ob *pool = v->pool, *loop = v->loop;
  v->pool = loop, v->loop = pool;
  copy_from(v, pool, pool + have);
  size_t t2 = v->t0 = clock(),
         vim = t2 == t1 ? MaxVim : (t2 - t0) / (t2 - t1),
         want = have,
         need = have - (Avail - req);

  // if we're too slow or small then grow
  if (want < need || vim < MinVim)
    do want <<= 1, vim <<= 1;
    while (want < need || vim < MinVim);

  // else if we're too big and fast then shrink
  else if (want >> 1 > need && vim > MaxVim)
    do want >>= 1, vim >>= 1;
    while (want >> 1 > need && vim > MaxVim);

  // else return ok
  else return true;

  // allocate a new pool
  ob *new = new_pool(want);

  // if it fails, succeed iff the first copy is big enough
  if (!new) return need <= have;

  // copy again, free old pool, return ok
  return v->len = want,
         v->pool = new,
         v->loop = new + want,
         copy_from(v, loop, loop + have),
         free(pool < loop ? pool : loop),
         v->t0 = clock(),
         true; }

static ob cp(li, ob, ob*, ob*);
static NoInline void copy_from(li v, ob *pool0, ob *top0) {
  size_t len1 = v->len;
  ob *sp0 = v->sp,
     *pool1 = v->pool,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->hp = v->cp = v->pool = pool1;

  // copy the stack
  ob *sp1 = v->sp = sp0 + shift;
  while (sp0 < top0) *sp1++ = cp(v, *sp0++, pool0, top0);

  // copy saved values
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // cheney's algorithm
  while (v->cp < v->hp) {
    mo k = (mo) v->cp;
    if (G(k) == act) data_walk[gettyp(k)](v, (ob) k, pool0, top0);
    else { // it's a function thread
      for (; G(k); k++) G(k) = (vm*) cp(v, (ob) G(k), pool0, top0);
      v->cp = (ob*) k + 2; } } }

static NoInline ob cp_mo(li v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  return d[1] = (vm*) dst, (ob) (src - ini + dst); }

static NoInline ob cp(li v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  mo src = (mo) x;
  x = (ob) G(src);
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    data_evac[gettyp(src)](v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }


static ob cp_str(li v, ob x, ob *pool0, ob *top0) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  return memcpy(dst, src, sizeof(struct str) + src->len),
         src->act = (vm*) dst,
         (ob) dst; }

static ob cp_two(li v, ob x, ob *pool0, ob *top0) {
  two src = (two) x, dst = bump(v, Width(struct two));
  return src->act = (vm*) dst,
         (ob) two_ini(dst, src->a, src->b); }

void wk_str(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

void wk_two(li v, ob x, ob *pool0, ob *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }

// function functions
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.

// allocate a thread
mo mo_n(li v, size_t n) {
  mo k = cells(v, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

static NoInline mo thdr(li v, size_t n, va_list xs) {
  vm *x = va_arg(xs, vm*);
  if (!x) return mo_n(v, n);
  mo k; with(x, k = thdr(v, n + 1, xs));
  if (k) k[n] = x;
  return k; }

NoInline mo thd(li v, ...) {
  mo k; va_list xs; return
    va_start(xs, v),
    k = thdr(v, 0, xs),
    va_end(xs),
    k; }

NoInline two pair(li v, ob a, ob b) {
  if (Avail < Width(struct two)) {
    bool ok; with(a, with(b, ok = please(v, Width(struct two))));
    if (!ok) return NULL; }
  return two_ini(bump(v, Width(struct two)), a, b); }

str strof(li v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, Width(struct str) + b2w(bs));
  if (o) memcpy(str_ini(o, bs)->text, c, bs);
  return o; }
