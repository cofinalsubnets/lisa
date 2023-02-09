#include "i.h"
static ob *new_pool(size_t n) { return malloc(n * 2 * sizeof(ob)); }

NoInline enum status li_go(li v) {
  mo ip; frame fp; ob xp, *hp, *sp;
  return Unpack(), ApY(ip, xp); }

void li_unwind(li v) {
  // reset the stack
  v->fp = (frame) (v->pool + v->len);
  v->sp = (ob*) v->fp; }

void li_fin(struct V *v) { if (v)
  free(v->pool < v->loop ? v->pool : v->loop),
  v->pool = v->loop = NULL; }

static bool li_ini_env(la);

// initialize a state
NoInline enum status li_ini(li v) {
  memset(v, 0, sizeof(struct V));
  const size_t len = 1 << 10; // expected to be a power of 2
  ob *pool = new_pool(len);
  if (pool) {
    v->len = len,
    v->pool = v->hp = pool,
    v->loop = pool + len,
    v->fp = (sf) (v->sp = pool + len),
    v->rand = v->t0 = clock();
    if (li_ini_env(v)) return Ok;
    li_fin(v); }

  return OomError; }

// helper function for making symbols
static sym symofs(la v, const char *c) {
  str s = strof(v, c);
  return s ? symof(v, s) : 0; }

static NoInline bool defprim(struct V *v, vm *i, const char *n) {
  sym y = symofs(v, n);
  mo k = y ? thd(v, i, y, End) : 0;
  return k && !!tbl_set(v, v->lex->topl, (ob) GF(k), (ob) k); }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline bool inst(la v, const char *a, vm *b) {
  sym z = symofs(v, a);
  return z && !!tbl_set(v, v->lex->topl, (ob) z, (ob) b); }

#define RegisterFunction(go, nom) && defprim(v, go, nom)
#define RegisterInstruction(a) && inst(v, "i-"#a, a)

static bool li_ini_env(struct V* v) {
  struct glob *l;
  struct sym *y;
  ob _; return
    (l = (struct glob*) mo_n(v, Width(struct glob))) &&
    (v->lex = setw(l, nil, Width(struct glob)),
     y = symofs(v, "ev"), v->lex->eval = y) &&
    (y = symofs(v, ":"), v->lex->define = y) &&
    (y = symofs(v, "?"), v->lex->cond = y) &&
    (y = symofs(v, "\\"), v->lex->lambda = y) &&
    (y = symofs(v, "`"), v->lex->quote = y) &&
    (y = symofs(v, ","), v->lex->begin = y) &&
    (y = symofs(v, "."), v->lex->splat = y) &&
    (v->lex->topl = tbl_new(v)) &&
    (v->lex->macros = tbl_new(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->lex->topl, _, (ob) v->lex->topl) &&
    (_ = (ob) symofs(v, "macros")) &&
    tbl_set(v, v->lex->topl, _, (ob) v->lex->macros)
    ForEachFunction(RegisterFunction)
    ForEachInstruction(RegisterInstruction); }

static void copy_from(li, ob*, ob*);

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

static NoInline void copy_from(li v, ob *pool0, ob *top0) {
  size_t len1 = v->len;
  ob *sp0 = v->sp,
     *pool1 = v->pool,
     *top1 = pool1 + len1,
     shift = top1 - top0;

  // reset state
  v->syms = 0;
  v->hp = v->cp = v->pool = pool1;
  v->sp = sp0 + shift;
  v->fp = (sf) ((ob*) v->fp + shift);

  v->xp = cp(v, v->xp, pool0, top0);
  v->ip = (mo) cp(v, (ob) v->ip, pool0, top0);

  // copy globals
  v->lex = (struct glob*) cp(v, (ob) v->lex, pool0, top0);
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);

  // copy the stack
  ob *sp = v->sp;
  frame fp = v->fp;
  for (;;) {
    while (sp < (ob*) fp) *sp++ = cp(v, *sp0++, pool0, top0);
    if (sp0 == top0) break;
    sf fp0 = (sf) sp0;
    fp->argc = fp0->argc;
    fp->subd = (sf) ((ob*) fp0->subd + shift);
    fp->clos = (ob*) cp(v, (ob) fp0->clos, pool0, top0);
    fp->retp = (mo) cp(v, (ob) fp0->retp, pool0, top0);
    sp = fp->argv;
    sp0 = fp0->argv;
    fp = fp->subd; }

  // cheney's alg
  while (v->cp < v->hp) {
    mo k = (mo) v->cp;
    if (G(k) == act) gettyp(k)->walk(v, (ob) k, pool0, top0);
    else { // it's a function thread
      for (; G(k); k++) G(k) = (vm*) cp(v, (ob) G(k), pool0, top0);
      v->cp = (ob*) k + 2; } } }


static NoInline ob cp_mo(li v, mo src, ob *pool0, ob *top0) {
  struct tag *fin = mo_tag(src);
  mo ini = fin->head,
     dst = bump(v, fin->end - ini),
     d = dst;
  for (mo s = ini; (G(d) = G(s)); G(s++) = (vm*) d++);
  return GF(d) = (vm*) dst,
         (ob) (src - ini + dst); }

NoInline ob cp(la v, ob x, ob *pool0, ob *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  mo src = (mo) x;
  x = (ob) G(src);
  if (!nump(x) && livep(v, x)) return x;
  if ((vm*) x == act) return
    gettyp(src)->evac(v, (ob) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }

// allocate a thread
mo mo_n(la v, U n) {
  mo k = cells(v, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

// push things onto the stack
static NoInline bool pushsr(la v, size_t i, va_list xs) {
  bool _; ob x = va_arg(xs, ob);
  return !x ? Avail >= i || please(v, i) :
    (with(x, _ = pushsr(v, i+1, xs)),
     _ && (*--v->sp = x, true)); }

NoInline bool pushs(la v, ...) {
  bool _; va_list xs; return
    va_start(xs, v),
    _ = pushsr(v, 0, xs),
    va_end(xs),
    _; }

static NoInline mo thdr(li v, size_t n, va_list xs) {
  vm *x = va_arg(xs, vm*);
  if (!x) return mo_n(v, n);
  mo k; with(x, k = thdr(v, n + 1, xs));
  if (k) k[n].ap = x;
  return k; }

NoInline mo thd(li v, ...) {
  mo k; va_list xs; return
    va_start(xs, v),
    k = thdr(v, 0, xs),
    va_end(xs),
    k; }

static NoInline two pair_gc(la v, ob a, ob b) {
  bool ok; with(a, with(b, ok = please(v, Width(struct two))));
  return ok ? pair(v, a, b) : 0; }

NoInline two pair(la v, ob a, ob b) {
  if (Avail < Width(struct two)) return pair_gc(v, a, b);
  return two_ini(bump(v, Width(struct two)), a, b); }
