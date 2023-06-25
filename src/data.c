#include "i.h"

static void wk_two(state, ob, ob*, ob*), wk_str(state, ob, ob*, ob*),
            tx_two(state, FILE*, ob), tx_str(state, FILE*, ob);
static bool eq_two(state, ob, ob), eq_str(state, ob, ob);
static ob cp_two(state, ob, ob*, ob*), cp_str(state, ob, ob*, ob*), cp(state, ob, ob*, ob*);

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

bool eql(state v, word a, word b) { return a == b ||
  (!nump(a|b) && datp((verb) a) && gettyp((verb) a)->equi(v, a, b)); }

// FIXME can overflow the stack
static bool eq_two(state v, word x, word y) { return
  htwop((verb) y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }

static bool eq_str(state v, word x, word y) {
  if (!hstrp((verb) y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len && !strncmp(a->text, b->text, a->len); }

struct methods
  two_methods = { .evac = cp_two, .walk = wk_two, .emit = tx_two, .equi = eq_two, },
  str_methods = { .evac = cp_str, .walk = wk_str, .emit = tx_str, .equi = eq_str, };

static NoInline ob push1_gc(state l, ob x) {
  bool ok; avec(l, x, ok = please(l, 1));
  return ok ? push1(l, x) : 0; }
static NoInline ob push2_gc(state l, ob x, ob y) {
  bool ok; avec(l, x, avec(l, y, ok = please(l, 2)));
  return ok ? push2(l, x, y) : 0; }
static NoInline two pair_gc(state f, word a, word b) {
  bool ok; avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
  return ok ? pair(f, a, b) : 0; }

ob push1(state l, ob x) { return
  avail(l) ? *--l->sp = x : push1_gc(l, x); }
ob push2(state l, ob x, ob y) {
  if (avail(l) < 2) return push2_gc(l, x, y);
  word *sp = l->sp -= 2;
  return sp[1] = y, sp[0] = x; }
two pair(state f, ob a, ob b) { return
  avail(f) < Width(struct two) ? pair_gc(f, a, b) :
    two_ini(bump(f, Width(struct two)), a, b); }

str strof(state f, const char* c) {
  size_t bs = strlen(c);
  str o = cells(f, Width(struct str) + b2w(bs));
  if (o) memcpy(str_ini(o, bs)->text, c, bs);
  return o; }

str str_ini(void *_, size_t len) {
  str s = _; return
    s->act = data, s->typ = &str_methods,
    s->len = len, s; }

two two_ini(void *_, word a, word b) {
  two w = _; return
    w->act = data, w->typ = &two_methods,
    w->_[0] = a, w->_[1] = b, w; }

static word cp_str(state v, word x, word *p0, word *t0) {
  str src = (str) x, dst = bump(v, Width(struct str) + b2w(src->len));
  return (word) (src->act = memcpy(dst, src, sizeof(struct str) + src->len)); }

static word cp_two(state v, word x, word *p0, word *t0) {
  two src = (two) x, dst = bump(v, Width(struct two));
  return (word) (src->act = (vm*) two_ini(dst, src->_[0], src->_[1])); }

static void wk_str(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

static void wk_two(state v, word x, word *p0, word *t0) {
  v->cp += Width(struct two), A(x) = cp(v, A(x), p0, t0), B(x) = cp(v, B(x), p0, t0); }

void *cells(state f, size_t n) { return
  n <= avail(f) || please(f, n) ? bump(f, n) : 0; }

NoInline Vm(gc, size_t n) { return Pack(),
  !please(f, n) ? OomError : f->ip->ap(f, f->ip, f->hp, f->sp); }

////
/// a simple copying garbage collector
//
// please : bool la size_t
// try to return with at least req words of available memory.
// return true on success, false otherwise. governs the heap size
// as a side effect by trying to keep
//   v = (t2 - t0) / (t2 - t1)
// between
#define v_lo 8
// and
#define v_hi (v_lo << 6)
// where
//       non-gc running time     t1    t2
//   ,.........................,/      |
//   -----------------------------------
//   |                          `------'
//   t0                  gc time (this cycle)
static void copy_from(state, word*, size_t);
#define too_little (len1 < req || v < v_lo)
#define too_big (len1 >> 1 > req && v > v_hi)
NoInline bool please(state f, size_t req) {
  size_t t1 = clock(), t0 = f->t0, len0 = f->len, len1 = len0;
  word *p0 = f->pool, *l0 = f->loop;
  f->pool = l0, f->loop = p0;
  copy_from(f, p0, len0);
  req += len0 - avail(f);
  size_t t2 = f->t0 = clock(),
         v = t2 == t1 ? v_hi : (t2 - t0) / (t2 - t1);
  if   (too_little) do len1 <<= 1, v <<= 1; while (too_little);
  else if (too_big) do len1 >>= 1, v >>= 1; while (too_big);
  else return true; // no resize is needed, so return success
  word *p1 = malloc(len1 * 2 * sizeof(word)); // else allocate a new pool
  return !p1 ? req <= len0 : // if that fails, succeed iff the first copy is big enough
    (f->loop = (f->pool = p1) + (f->len = len1), // else copy again & return success
     copy_from(f, l0, len0),
     free(p0 < l0 ? p0 : l0),
     f->t0 = clock(),
     true); }

static NoInline void copy_from(state f, word *p0, size_t len0) {
  size_t len1 = f->len, slen;
  word *p1 = f->hp = f->cp = f->pool,
       *t0 = p0 + len0,
       *t1 = p1 + len1,
       *sp0 = f->sp,
       *sp1 = f->sp = t1 - (slen = t0 - sp0);
  f->ip = (verb) cp(f, (word) f->ip, p0, t0);
  // copy stack
  for (size_t i = 0; i < slen; i++) sp1[i] = cp(f, sp0[i], p0, t0);
  // copy managed values
  for (struct mm *r = f->safe; r; r = r->next) *r->addr = cp(f, *r->addr, p0, t0);
  // cheney's algorithm
  for (mo k; (k = (mo) f->cp) < (mo) f->hp;)
    if (datp(k)) gettyp(k)->walk(f, (ob) k, p0, t0);
    else { for (; k->ap; k++) k->x = cp(f, k->x, p0, t0);
           f->cp = (ob*) k + 2; } }

// this can give a false positive if x is a fixnum
static Inline bool livep(state v, word x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static NoInline word cp(state v, word x, word *p0, word *t0) {
  if (nump(x) || (ob*) x < p0 || (ob*) x >= t0) return x;
  verb src = (verb) x;
  if (homp(src->x) && livep(v, src->x)) return src->x;
  if (datp(src)) return gettyp(src)->evac(v, (word) src, p0, t0);
  struct tag *t = mo_tag(src);
  verb ini = t->head,
       dst = bump(v, t->end - ini),
       d = dst;
  for (verb s = ini; (d->x = s->x); s++->x = (ob) d++);
  return d[1].ap = (vm*) dst, (ob) (src - ini + dst); }
