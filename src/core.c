#include "i.h"
typedef uintptr_t size;

#define Vm(n, ...) enum status n(state f, verb ip, word *hp, word *sp, ##__VA_ARGS__)
typedef Vm(vm);

struct tag {
  void **null;
  mo head;
  union mo end[]; };


static bool
  eql(state, word, word),
  please(state, size),
  eq_two(state, ob, ob),
  eq_str(state, ob, ob);

static Vm(gc, size_t);

static ob
  cp_two(O, ob, ob*, ob*),
  cp_str(O, ob, ob*, ob*);

static bool
  push3(state, word, word, word);

static Inline void *bump(state f, size n) {
  void *x = f->hp; return f->hp += n, x; }

#define avail(f) (f->sp-f->hp)

void *cells(state f, size_t n) {
  return n <= avail(f) || please(f, n) ? bump(f, n) : 0; }

static NoInline bool push2_gc(state f, word x, word y) {
  bool ok; avec(f, x, avec(f, y, ok = please(f, 2)));
  return ok && push2(f, x, y); }
bool push2(state f, word x, word y) {
  return avail(f) < 2 ? push2_gc(f, x, y) :
    (f->sp -= 2, f->sp[0] = x, f->sp[1] = y, true); }

static NoInline bool push3_gc(state f, word x, word y, word z) {
  bool ok; avec(f, x, avec(f, y, avec(f, z, ok = please(f, 3))));
  return ok && push3(f, x, y, z); }
static bool push3(state f, word x, word y, word z) {
  return avail(f) < 3 ? push3_gc(f, x, y, z) :
    (f->sp -= 3, f->sp[0] = x, f->sp[1] = y, f->sp[2] = z, true); }

// list length
static size_t llen(ob l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static two two_ini(void*, ob, ob);
static verb mo_ini(void*, size_t);
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
static verb mo_n(state f, size n) {
  verb k = cells(f, n + Width(struct tag));
  return !k ? k : mo_ini(k, n); }

struct cctx {
  word s1, s2, ib, sb, sn;
  struct cctx *par; };

static struct cctx *scope(state f, struct cctx **par) {
  struct cctx *sc = (struct cctx *) mo_n(f, Width(struct cctx));
  if (sc) sc->s1 = sc->s2 = sc->ib = sc->sb = sc->sn = nil,
          sc->par = par ? *par : (struct cctx*) nil;
  return sc; }

static Inline struct tag *mo_tag(verb k) {
  while (k->x) k++;
  return (struct tag*) k; }

static word lidx(state f, ob l, ob x) {
  for (word i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }

static vm ap, K, cur, ret, rec, yield, var, br, jump;
#define Ana(n) size n(state f, struct cctx**c, size m, word x)
#define Cata(n) verb n(state f, struct cctx **c, verb k)
typedef Ana(ca); typedef Cata(cc);

static Cata(yield_thread) { return k; }
static Cata(pull_thread) { return ((cc*) (*f->sp++))(f, c, k); }
static Cata(e1) { return (--k)->x = *f->sp++, pull_thread(f, c, k); }
static Cata(e2) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static Cata(em_ap) {
  if (k->ap == ret) k->ap = rec;
  else (--k)->ap = ap;
  return pull_thread(f, c, k); }

static Cata(em_var) {
  word sym = *f->sp++,
       idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  return (--k)->x = putnum(idx),
         (--k)->ap = var,
         pull_thread(f, c, k); }

static verb cata(O f, struct cctx **c, size m) {
  verb k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(word));
  return pull_thread(f, c, k + m); }

static ca ana, ana_list, ana_str, ana_ap;
NoInline status eval(state f, word x) {
  struct cctx *c = push2(f, x, (word) yield_thread) ? scope(f, NULL) : NULL;
  verb k = 0;
  if (c) {
    size m;
    avec(f, c,
      m = ana(f, &c, 1, pop1(f)),
      m = m && push2(f, (word) e1, (word) yield) ? m : 0,
      k = m ? cata(f, &c, m) : k); }
  return !k ? OomError : k->ap(f, k, f->hp, f->sp); }

static Ana(value) {
  return push3(f, (word) e2, (word) K, x) ? m + 2 : 0; }

static Ana(ana) { return
  twop(x) ? ana_list(f, c, m, x) :
  strp(x) ? ana_str(f, c, m, x) :
            value(f, c, m, x); }

static Ana(ana_str) {
  if (nilp((word) (*c)->par)) return value(f, c, m, x);
  if (lidx(f, (*c)->sb, x) < 0) {
    x = (word) pair(f, x, (*c)->sb);
    if (!x) return x;
    (*c)->sb = x;
    x = (word) pair(f, A(x), (*c)->ib);
    if (!x) return x;
    (*c)->ib = x;
    x = A(x); }
  return push3(f, (word) em_var, x, (*c)->sn) ? m + 2 : 0; }

static cc
  ana_cond_push_continuation,
  ana_cond_push_alternative,
  ana_cond_pop_alternative,
  ana_cond_pop_continuation;

static Cata(ana_cond_pop_alternative) {
  (--k)->x = A((*c)->s1);
  (--k)->ap = br;
  (*c)->s1 = B((*c)->s1);
  return pull_thread(f, c, k); }

static Ana(ana_cond) {
  if (!push2(f, x, (word) ana_cond_pop_continuation)) return 0;
  x = pop1(f);
  MM(f, &x);
  for (; m; x = B(B(x))) {
    if (!twop(x)) {
      x = (ob) pair(f, x, nil);
      if (!x) { m = 0; break; } }
    if (!twop(B(x))) {
      m = ana(f, c, m, A(x));
      break; }
    m = ana(f, c, m + 2, A(x));
    m = m && push1(f, (word) ana_cond_pop_alternative) ? m : 0;
    m = m ? ana(f, c, m + 2, A(B(x))): 0;
    m = m && push1(f, (word) ana_cond_push_alternative) ? m + 2 : 0; }
  UM(f);
  return m && push1(f, (word) ana_cond_push_continuation) ? m : 0; }

// reverse decons: pushes last list item to stack, returns init of list.
static word snoced(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  ob y = A(x);
  avec(f, y, x = snoced(f, B(x)));
  return x ? (word) pair(f, y, x) : x; }

static Ana(ana_lambda) {
  if (!(x = snoced(f, x)) || !push1(f, x)) return 0;
  struct cctx *d = scope(f, c);
  if (!d) return 0;
  d->sb = pop1(f);
  MM(f, &d);
  size inner_m = push2(f, pop1(f), (word) yield_thread) ? ana(f, &d, 4, pop1(f)) : 0;
  if (inner_m) {
    size sbn = llen(d->sb);
    verb k = push3(f, (word) e2, (word) ret, putnum(sbn)) ? cata(f, &d, inner_m) : 0;
    if (k) {
      if (sbn > 1) k -= 2, k[0].ap = cur, k[1].x = putnum(sbn);
      struct tag *t = mo_tag(k);
      t->head = k; }
    x = k && twop(d->ib) ? (word) pair(f, (word) k, d->ib) : (word) k; }
  UM(f);
  return x ? ana(f, c, m, x) : x; }

static Ana(ana_quote) { return value(f, c, m, twop(x) ? A(x) : x); }

static bool kstrq(str s0, const char *s1) { return
  strlen(s1) == s0->len &&
  0 == strncmp(s0->text, s1, s0->len); }

#define Cond "?"
#define Lambda "\\"
static Ana(ana_list) {
  if (strp(A(x))) {
    str s = (str) A(x);
    if (kstrq(s, Quote)) return ana_quote(f, c, m, B(x));
    if (kstrq(s, Cond)) return ana_cond(f, c, m, B(x));
    if (kstrq(s, Lambda)) return ana_lambda(f, c, m, B(x)); }
  return ana_ap(f, c, m, x); }

static Ana(ana_ap) {
  MM(f, &x);
  m = ana(f, c, m, A(x));
  (*c)->sn += 2;
  while (m && twop(x = B(x)))
    m = ana(f, c, m + 1, A(x)),
    m = m && push1(f, (word) em_ap) ? m : 0;
  (*c)->sn -= 2;
  UM(f);
  return m; }

static Cata(ana_cond_push_continuation) {
  two w = pair(f, (word) k, (*c)->s2);
  return !w ? (verb) w :
    pull_thread(f, c, (verb) A((*c)->s2 = (word) w)); }

static Cata(ana_cond_push_alternative) {
  two w = pair(f, (word) k, (*c)->s1);
  if (!w) return (verb) w;
  k = (verb) A((*c)->s1 = (word) w) - 2;
  verb kk = (verb) A((*c)->s2);
  if (kk->ap == ret || kk->ap == rec) // if the destination is a return or a tail call
    k[0].ap = kk->ap, k[1].x = kk[1].x; // then forward it instead of emitting a jump.
  else k[0].ap = jump, k[1].m = kk;
  return pull_thread(f, c, k); }

static Cata(ana_cond_pop_continuation) {
  return (*c)->s2 = B((*c)->s2),
         pull_thread(f, c, k); }

struct methods
  two_methods = { .evac = cp_two, .walk = wk_two, .emit = tx_two, .equi = eq_two, },
  str_methods = { .evac = cp_str, .walk = wk_str, .emit = tx_str, .equi = eq_str, };

static bool eql(state v, word a, word b) { return a == b ||
  (!nump(a|b) && datp((verb) a) && gettyp((verb) a)->equi(v, a, b)); }

static bool eq_two(state v, word x, word y) { // FIXME can overflow stack
  return htwop((verb) y) && eql(v, A(x), A(y)) && eql(v, B(x), B(y)); }

static bool eq_str(state v, word x, word y) {
  if (!hstrp((verb) y)) return false;
  str a = (str) x, b = (str) y;
  return a->len == b->len &&
    !strncmp(a->text, b->text, a->len); }

intptr_t liprng(intptr_t n) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (1 + n * steele_vigna_2021) >> sizeof(intptr_t); }

static NoInline word push1_gc(state f, word x) {
  bool ok; return avec(f, x, ok = please(f, 1)),
                  ok ? *--f->sp = x : 0; }

word push1(state f, word x) {
  return avail(f) ? *--f->sp = x : push1_gc(f, x); }

void li_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

enum status li_ini(struct l_state *f) {
  memset(f, 0, sizeof(struct l_state));
  const size_t len0 = 1; // a power of 2
  ob *pool = malloc(len0 * 2 * sizeof(intptr_t));
  if (!pool) return OomError;
  f->loop = f->sp = (f->pool = f->hp = pool) + (f->len = len0);
  f->t0 = clock();
  return Ok; }

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

str str_ini(void *_, size len) {
  str s = _; return
    s->act = data,
    s->typ = &str_methods,
    s->len = len,
    s; }

static two two_ini(void *_, word a, word b) {
  two w = _; return
    w->act = data,
    w->typ = &two_methods,
    w->_[0] = a,
    w->_[1] = b,
    w; }

static verb mo_ini(void *_, size len) {
  struct tag *t = (struct tag*) ((verb) _ + len);
  return t->null = NULL, t->head = _; }

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
static NoInline bool please(state f, size req) {
  size t1 = clock(), t0 = f->t0, have = f->len;
  word *pool = f->pool, *loop = f->loop;
  f->pool = loop, f->loop = pool;
  copy_from(f, pool, pool + have);
  size t2 = f->t0 = clock(),
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

static word cp_str(state v, word x, word *pool0, word *top0) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  return (word) (src->act = (vm*) dst); }

static word cp_two(state v, word x, word *pool0, word *top0) {
  two src = (two) x,
      dst = two_ini(bump(v, Width(struct two)), src->_[0], src->_[1]);
  return (word) (src->act = (vm*) dst); }

void wk_str(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

void wk_two(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }
enum status l_evals(struct l_state *f, const char *prog) {
  enum status s = receive2(f, prog);
  return s != Ok ? s : eval(f, pop1(f)); }
#define shew(s,x) (printf(s),transmit(f,stdout,x),puts(""))
// echo loop
static enum status go(state f) {
  printf("# dim=%ld f@0x%lx[len=%ld]\n", sizeof(word), (word) f, f->len);
#ifdef testing
  self_test(f);
#endif
  // echo loop
  intptr_t s, height = f->pool + f->len - f->sp;
  while ((s = receive(f, stdin)) != Eof) {
    if (s == Ok && (s = eval(f, pop1(f))) == Ok)
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout);
    else
      fprintf(stderr, "# status %ld\n", s),
      f->sp = f->pool + f->len - height; }
  return Ok; }

int main(int ac, char **av) {
  state f = &((struct l_state){});
  status s = li_ini(f);
  if (s == Ok) s = go(f), li_fin(f);
  return s; }

#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
static NoInline Vm(gc, size_t n) { return Pack(),
  !please(f, n) ? OomError : f->ip->ap(f, f->ip, f->hp, f->sp); }

static Vm(rec) {
  word x = sp[0], j = sp[1];
  sp += getnum(ip[1].x) + 1;
  if (nump(j)) ip = (verb) *++sp, *sp = j;
  else ip = (verb) j, *sp = x;
  return ip->ap(f, ip, hp, sp); }

#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)

static Vm(var) { Have1(); return
  sp[-1] = sp[getnum(ip[1].x)],
  ip[2].ap(f, ip + 2, hp, sp - 1); }

static Vm(br) { return
  ip = nilp(*sp) ? ip[1].m : ip + 2,
  ip->ap(f, ip, hp, sp + 1); }

static Vm(jump) { return ip[1].m->ap(f, ip[1].m, hp, sp); }

static Vm(yield) { return Pack(), Ok; }

static Vm(Kj) { Have1(); return
  *--sp = ip[1].x,
  ip = ip[2].m,
  ip->ap(f, ip, hp, sp); }

static Vm(ret) { word r = *sp; return
  sp += getnum(ip[1].x) + 1,
  ip = (verb) *sp,
  *sp = r,
  ip->ap(f, ip, hp, sp); }

static Vm(ap) {
  if (nump(sp[1])) return ip[1].ap(f, ip + 1, hp, sp + 1);
  verb k = (verb) sp[1]; return
    sp[1] = (word) (ip + 1),
    k->ap(f, k, hp, sp); }

static Vm(cur) {
  intptr_t n = getnum(ip[1].x);
  // XXX base case of 1 is wasteful
  if (n == 1) return ip += 2, ip->ap(f, ip, hp, sp);
  const size_t S = 5 + Width(struct tag);
  Have(S);
  verb k = (verb) hp;
  hp += S;
  k[0].ap = cur;
  k[1].x = putnum(n - 1);
  k[2].ap = Kj;
  k[3].x = *sp++;
  k[4].m = ip + 2;
  k[5].x = 0;
  k[6].m = k;
  ip = (verb) *sp;
  *sp = (word) k;
  return ip->ap(f, ip, hp, sp); }

Vm(data) { word r = (word) ip; return
  ip = (verb) *++sp,
  *sp = r,
  ip->ap(f, ip, hp, sp); }

static Vm(K) { Have1(); return
  sp[-1] = ip[1].x,
  ip[2].ap(f, ip + 2, hp, sp - 1); }
