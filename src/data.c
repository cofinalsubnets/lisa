#include "i.h"

static void
  wk_two(state, ob, ob*, ob*),
  wk_str(state, ob, ob*, ob*),
  tx_two(state, FILE*, ob),
  tx_str(state, FILE*, ob);

static void tx_str(state v, FILE *o, word _) {
  str s = (str) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static void tx_two(state v, FILE *o, word x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

static bool
  eq_two(state, ob, ob),
  eq_str(state, ob, ob);
static ob
  cp_two(state, ob, ob*, ob*),
  cp_str(state, ob, ob*, ob*);

bool eql(state v, word a, word b) { return a == b ||
  (!nump(a|b) && datp((verb) a) && gettyp((verb) a)->equi(v, a, b)); }

static bool eq_two(state v, word x, word y) { // FIXME can overflow stack
  return htwop((verb) y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }

static bool eq_str(state v, word x, word y) {
  if (!hstrp((verb) y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len &&
    !strncmp(a->text, b->text, a->len); }

struct methods
  two_methods = { .evac = cp_two, .walk = wk_two, .emit = tx_two, .equi = eq_two, },
  str_methods = { .evac = cp_str, .walk = wk_str, .emit = tx_str, .equi = eq_str, };

static NoInline word pushnr(state l, size_t n, size_t m, va_list xs) {
  if (m == 0) return please(l, n);
  ob x = va_arg(xs, ob), y;
  avec(l, x, y = pushnr(l, n, m - 1, xs));
  return y ? *--l->sp = x : y; }

NoInline word pushn(state l, size_t n, ...) {
  word r = 0; va_list xs; va_start(xs, n);
  if (avail(l) < n) r = pushnr(l, n, n, xs);
  else for (word *sp = (l->sp -= n); n--; *sp++ = r = va_arg(xs, word));
  return va_end(xs), r; }

static NoInline two pair_gc(state f, word a, word b) {
  bool ok; return
    avec(f, a, avec(f, b, ok = please(f, Width(struct two)))),
    ok ? pair(f, a, b) : 0; }

two pair(state f, ob a, ob b) {
  return avail(f) < Width(struct two) ? pair_gc(f, a, b) :
    two_ini(bump(f, Width(struct two)), a, b); }

str strof(state f, const char* c) {
  size_t bs = strlen(c);
  str o = cells(f, Width(struct str) + b2w(bs));
  if (o) memcpy(str_ini(o, bs)->text, c, bs);
  return o; }

str str_ini(void *_, size_t len) {
  str s = _; return
    s->act = data,
    s->typ = &str_methods,
    s->len = len,
    s; }

two two_ini(void *_, word a, word b) {
  two w = _; return
    w->act = data,
    w->typ = &two_methods,
    w->_[0] = a,
    w->_[1] = b,
    w; }

////
/// a simple copying garbage collector
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. governs the heap size
// as a side effect by trying to keep
//   vim = (t2 - t0) / (t2 - t1)
// between
#define vim_inf 8
// and
#define vim_sup (vim_inf << 6)
// where
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static void copy_from(state, word*, word*);
#define too_little (want < need || vim < vim_inf)
#define too_big (want >> 1 > need && vim > vim_sup)
#define grow() want <<= 1, vim <<= 1
#define shrink() want >>= 1, vim >>= 1
NoInline bool please(state f, size_t req) {
  size_t t1 = clock(), t0 = f->t0, have = f->len;
  word *pool = f->pool, *loop = f->loop;
  f->pool = loop, f->loop = pool;
  copy_from(f, pool, pool + have);
  size_t t2 = f->t0 = clock(),
       vim = t2 == t1 ? vim_sup : (t2 - t0) / (t2 - t1),
       want = have,
       need = have - (avail(f) - req);
  if   (too_little) do grow(); while (too_little);
  else if (too_big) do shrink(); while (too_big);
  else return true; // no resize is needed, so return success
  // try and resize
  //
  word *new = malloc(want * 2 * sizeof(word)); // allocate a new pool
  if (!new) return need <= have; // if that fails, succeed iff the first copy is big enough
  // we got a new pool; copy again, free the old pool, return ok
  f->loop = (f->pool = new) + (f->len = want);
  copy_from(f, loop, loop + have);
  free(pool < loop ? pool : loop);
  f->t0 = clock();
  return true; }

static word cp(state, word, word*, word*);
static NoInline void copy_from(state f, word *pool0, word *top0) {
  size_t len1 = f->len;
  word *sp0 = f->sp,
       *pool1 = f->pool,
       *top1 = pool1 + len1;
  size_t slen = top0 - sp0;
  // reset heap
  f->hp = f->cp = pool1;
  // copy stack
  word *sp1 = f->sp = top1 - slen;
  for (size_t i = 0; i < slen; i++)
    sp1[i] = cp(f, sp0[i], pool0, top0);
  // copy registers
  f->ip = (verb) cp(f, (word) f->ip, pool0, top0);
  // copy user values
  for (struct ll *r = f->safe; r; r = r->next)
    *r->addr = cp(f, *r->addr, pool0, top0);

  // cheney's algorithm
  for (mo k; (k = (mo) f->cp) < (mo) f->hp;)
    if (datp(k)) gettyp(k)->walk(f, (ob) k, pool0, top0);
    else { for (; k->ap; k++) k->x = cp(f, k->x, pool0, top0);
           f->cp = (ob*) k + 2; } }


static word cp_mo(state v, verb src, word *pool0, word *top0) {
  struct tag *t = mo_tag(src);
  verb ini = t->head,
       dst = bump(v, t->end - ini),
       d = dst;
  for (verb s = ini; (d->x = s->x); s++->x = (ob) d++);
  return (d+1)->ap = (vm*) dst,
         (ob) (src - ini + dst); }

// this can give a false positive if x is a fixnum
static Inline bool livep(state v, word x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static NoInline word cp(state v, word x, word *pool0, word *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  verb src = (verb) x;
  if (homp(src->x) && livep(v, src->x)) return src->x;
  else if (datp(src)) return gettyp(src)->evac(v, (word) src, pool0, top0);
  else return cp_mo(v, src, pool0, top0); }

word cp_str(state v, word x, word *pool0, word *top0) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  return (word) (src->act = (vm*) dst); }

word cp_two(state v, word x, word *pool0, word *top0) {
  two src = (two) x,
      dst = two_ini(bump(v, Width(struct two)), src->_[0], src->_[1]);
  return (word) (src->act = (vm*) dst); }

void wk_str(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

void wk_two(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }

void *cells(state f, size_t n) {
  return n <= avail(f) || please(f, n) ? bump(f, n) : 0; }

NoInline Vm(gc, size_t n) { return Pack(),
  !please(f, n) ? OomError : f->ip->ap(f, f->ip, f->hp, f->sp); }
