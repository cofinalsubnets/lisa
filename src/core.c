#include "i.h"
#include <stdio.h>
#include <errno.h>

typedef enum status vm(state, verb, word*, word*);
static vm apply, K, curry, retn;
void transmit(state, FILE*, word);

struct str {
  status (*act)(state, verb, word*, word*);
  struct methods *typ;
  uintptr_t len;
  char text[]; };

struct tag {
  void **null;
  mo head;
  union mo end[]; };

// align bytes up to the nearest word
static Inline size b2w(size b) {
  ldiv_t _ = ldiv(b, sizeof(word));
  return _.quot + (_.rem ? 1 : 0); }

struct methods {
  word (*evac)(state, ob, ob*, ob*);
  void (*walk)(state, ob, ob*, ob*),
       (*emit)(state, FILE*, ob);
  bool (*equi)(state, ob, ob); };

static Inline void *bump(state f, size n) {
  void *x = f->hp;
  f->hp += n;
  assert(f->hp <= f->sp);
  return x; }

static bool eql(state, word, word), please(state, size),
            eq_two(O, ob, ob), eq_str(O, ob, ob);

#define avail(f) (f->sp-f->hp)
#define Width(_) b2w(sizeof(_))
static void *cells(state f, size n) {
  return avail(f) < n && !please(f, n) ? 0 : bump(f, n); }

static enum status gc(state, verb, word*, word*, size);
static void wk_two(O, ob, ob*, ob*), wk_str(O, ob, ob*, ob*),
     tx_two(O, FILE*, ob), tx_str(O, FILE*, ob);
static ob cp_two(O, ob, ob*, ob*), cp_str(O, ob, ob*, ob*);

static bool push2(state, word, word), push3(state, word, word, word);

static NoInline bool push2_gc(state f, word x, word y) {
  bool ok; avec(f, x, avec(f, y, ok = please(f, 2)));
  return ok && push2(f, x, y); }
static bool push2(state f, word x, word y) {
  return avail(f) < 2 ? push2_gc(f, x, y) :
    (f->sp -= 2, f->sp[0] = x, f->sp[1] = y); }
static NoInline bool push3_gc(state f, word x, word y, word z) {
  bool ok; avec(f, x, avec(f, y, avec(f, z, ok = please(f, 3))));
  return ok && push3(f, x, y, z); }
static bool push3(state f, word x, word y, word z) {
  return avail(f) < 3 ? push3_gc(f, x, y, z) :
    (f->sp -= 3, f->sp[0] = x, f->sp[1] = y, f->sp[2] = z); }

// list length
static size llen(ob l) {
  size n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

static two two_ini(void*, ob, ob);
static str str_ini(void*, size);
static verb mo_ini(void*, size);
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
  if (sc)
    sc->s1 = sc->s2 = sc->ib = sc->sb = sc->sn = nil,
    sc->par = par ? *par : (struct cctx*) nil;
  return sc; }

static Inline struct tag *mo_tag(verb k) {
  while (k->x) k++;
  return (struct tag*) k; }

#define Cata(n) verb n(state f, struct cctx **c, verb k)
static Cata(yield_thread) { return k; }
static Cata(pull_thread) { return ((verb (*)(state, struct cctx**, verb)) (*f->sp++))(f, c, k); }
static Cata(e1) { return (--k)->x = *f->sp++, pull_thread(f, c, k); }
static Cata(e2) { return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static word lidx(state f, ob l, ob x) {
  for (word i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }

#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)
#define Vm(n) enum status n(state f, verb ip, word *hp, word *sp)
static Vm(recn) {
  word x = sp[0], j = sp[1];
  sp += getnum(ip[1].x) + 1;
  if (nump(j)) ip = (verb) *++sp, *sp = j;
  else ip = (verb) j, *sp = x;
  return ip->ap(f, ip, hp, sp); }

static Vm(ref) {
  Have1(); return
    sp[-1] = sp[getnum(ip[1].x)],
    ip[2].ap(f, ip + 2, hp, sp - 1); }

static Vm(branch) { return
  ip = nilp(*sp++) ? ip[1].m : ip + 2,
  ip->ap(f, ip, hp, sp); }

static Vm(jump) { return
  ip = ip[1].m, ip->ap(f, ip, hp, sp); }

static Vm(Kj) {
  Have1();
  *--sp = ip[1].x;
  ip = ip[2].m;
  return ip->ap(f, ip, hp, sp); }

Vm(retn) { word r = *sp; return
  sp += getnum(ip[1].x) + 1,
  ip = (verb) *sp,
  *sp = r,
  ip->ap(f, ip, hp, sp); }

Vm(apply) {
  if (nump(sp[1])) return ip[1].ap(f, ip + 1, hp, sp + 1);
  verb k = (verb) sp[1];
  sp[1] = (word) (ip + 1);
  return k->ap(f, k, hp, sp); }

static Vm(curry) {
  intptr_t n = getnum(ip[1].x);
  // XXX base case of 1 is wasteful
  if (n == 1) return ip += 2, ip->ap(f, ip, hp, sp);
  const size S = 5 + Width(struct tag);
  Have(S);
  verb k = (verb) hp;
  hp += S;
  k[0].ap = curry;
  k[1].x = putnum(n - 1);
  k[2].ap = Kj;
  k[3].x = *sp++;
  k[4].m = ip + 2;
  k[5].x = 0;
  k[6].m = k;
  ip = (verb) *sp;
  *sp = (word) k;
  return ip->ap(f, ip, hp, sp); }

Vm(data) {
  word x = (word) ip;
  ip = (verb) *++sp;
  *sp = x;
  return ip->ap(f, ip, hp, sp); }

static verb em_ap(state f, struct cctx **c, verb k) {
  if (k->ap == retn) k->ap = recn;
  else (--k)->ap = apply;
  return pull_thread(f, c, k); }

static verb em_ref(state f, struct cctx **c, verb k) {
  word sym = *f->sp++,
       idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  return (--k)->x = putnum(idx),
         (--k)->ap = ref,
         pull_thread(f, c, k); }

static size ana(state, struct cctx**, size, word);
static verb cata(O f, struct cctx **c, size m) {
  assert((*c)->sn == nil);
  verb k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(word));
  return pull_thread(f, c, k + m); }

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


bool kstrq(str s0, const char *s1) { return
  strlen(s1) == s0->len &&
  0 == strncmp(s0->text, s1, s0->len); }

typedef size ca(state, struct cctx**, size, word);
typedef verb cc(state, struct cctx**, verb);
#define Ana(n) size n(state f, struct cctx**c, size m, word x)
static Ana(value) {
  return push3(f, (word) e2, (word) K, x) ? m + 2 : 0; }

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
  return push3(f, (word) em_ref, x, (*c)->sn) ? m + 2 : 0; }

static Ana(ana_list);
static Ana(ana) { return
  twop(x) ? ana_list(f, c, m, x) :
  strp(x) ? ana_str(f, c, m, x) :
            value(f, c, m, x); }

static cc
  ana_cond_push_continuation,
  ana_cond_push_alternative,
  ana_cond_pop_alternative,
  ana_cond_pop_continuation;

static Cata(ana_cond_pop_alternative) {
  (--k)->x = A((*c)->s1);
  (--k)->ap = branch;
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
    verb k = push3(f, (word) e2, (word) retn, putnum(sbn)) ? cata(f, &d, inner_m) : 0;
    if (k) {
      if (sbn > 1) k -= 2, k[0].ap = curry, k[1].x = putnum(sbn);
      struct tag *t = mo_tag(k);
      assert(t->head <= k);
      t->head = k; }
    x = k && twop(d->ib) ? (word) pair(f, (word) k, d->ib) : (word) k; }
  UM(f);
  return x ? ana(f, c, m, x) : x; }

static Ana(ana_quote) { return value(f, c, m, twop(x) ? A(x) : x); }

static ca ana_ap;
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
  if (kk->ap == retn || kk->ap == recn) // if the destination is a return or a tail call
    k[0].ap = kk->ap, k[1].x = kk[1].x; // then forward it instead of emitting a jump.
  else k[0].ap = jump, k[1].m = kk;
  return pull_thread(f, c, k); }

static Cata(ana_cond_pop_continuation) {
  return (*c)->s2 = B((*c)->s2),
         pull_thread(f, c, k); }

Vm(yield) { return Pack(), Ok; }

Vm(K) {
  Have1();
  *--sp = ip[1].x;
  ip += 2;
  return ip->ap(f, ip, hp, sp); }

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

static NoInline status gc(state f, verb ip, word *hp, word *sp, size n) {
  return Pack(), please(f, n) ? li_go(f) : OomError; }

status li_go(state f) {
  return f->ip->ap(f, f->ip, f->hp, f->sp); }

void li_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

NoInline status li_ini(state f) {
  memset(f, 0, sizeof(struct carrier));
  const size_t len0 = 1; // a power of 2
  ob *pool = malloc(len0 * 2 * sizeof(intptr_t));
  if (!pool) return OomError;
  f->len = len0;
  f->pool = f->hp = pool;
  f->loop = f->sp = pool + len0;
  f->t0 = clock();
  return Ok; }

static NoInline two pair_gc(state f, word a, word b) {
  bool ok;
  avec(f, a, avec(f, b, ok = please(f, Width(struct two))));
  return !ok ? 0 : two_ini(bump(f, Width(struct two)), a, b); }

two pair(state f, ob a, ob b) {
  if (avail(f) < Width(struct two)) return pair_gc(f, a, b);
  return two_ini(bump(f, Width(struct two)), a, b); }

str strof(state f, const char* c) {
  size_t bs = strlen(c);
  str o = cells(f, Width(struct str) + b2w(bs));
  if (o) str_ini(o, bs),
         memcpy(o->text, c, bs);
  return o; }

static str str_ini(void *_, size len) {
  str s = _;
  s->act = data;
  s->typ = &str_methods;
  s->len = len;
  return s; }

static two two_ini(void *_, word a, word b) {
  two w = _;
  w->act = data;
  w->typ = &two_methods;
  w->_[0] = a;
  w->_[1] = b;
  return w; }

static verb mo_ini(void *_, size len) {
  struct tag *t = (struct tag*) ((verb) _ + len);
  t->null = NULL;
  return t->head = _; }

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
#define little (want < need || vim < vim_inf)
#define big (want >> 1 > need && vim > vim_sup)
static void copy_from(state, word*, word*);
static NoInline bool please(state f, size req) {
  size t1 = clock(), t0 = f->t0, have = f->len;
  word *pool = f->pool, *loop = f->loop;
  f->pool = loop, f->loop = pool;
  copy_from(f, pool, pool + have);
  size t2 = f->t0 = clock(),
       vim = t2 == t1 ? vim_sup : (t2 - t0) / (t2 - t1),
       want = have,
       need = have - (avail(f) - req);
  if   (little) do want <<= 1, vim <<= 1; while (little);
  else if (big) do want >>= 1, vim >>= 1; while (big);
  else return true; // else no resize is needed, so return success
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
  size len1 = f->len;
  word *sp0 = f->sp,
       *pool1 = f->pool,
       *top1 = pool1 + len1;
  size slen = top0 - sp0;
  // reset heap
  f->hp = f->cp = pool1;
  // copy stack
  word *sp1 = f->sp = top1 - slen;
  for (size i = 0; i < slen; i++)
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
           f->cp = (ob*) k + 2; }

  assert(f->pool + f->len - f->sp == top0 - sp0);
  assert(f->cp == f->hp); }

static NoInline word cp_mo(state v, verb src, word *pool0, word *top0) {
  struct tag *fin = mo_tag(src);
  verb ini = fin->head,
       dst = bump(v, fin->end - ini),
       d = dst;
  assert(ini);
  for (verb s = ini; (d->x = s->x); s++->x = (ob) d++);
  return (d+1)->ap = (void*) dst,
         (ob) (src - ini + dst); }

// this can give a false positive if x is a fixnum
static Inline bool livep(state v, word x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static NoInline word cp(state v, word x, word *pool0, word *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  verb src = (verb) x;
  if (homp(src->x) && livep(v, src->x)) x = src->x;
  else if (datp(src)) x = gettyp(src)->evac(v, (word) src, pool0, top0);
  else x = cp_mo(v, src, pool0, top0);
  assert(livep(v, x)); // XXX
  return x; }

static word cp_str(state v, word x, word *pool0, word *top0) {
  str src = (str) x,
      dst = bump(v, Width(struct str) + b2w(src->len));
  memcpy(dst, src, sizeof(struct str) + src->len);
  return (word) (src->act = (void*) dst); }

static word cp_two(state v, word x, word *pool0, word *top0) {
  two src = (two) x,
      dst = two_ini(bump(v, Width(struct two)), src->_[0], src->_[1]);
  return (word) (src->act = (void*) dst); }

static void wk_str(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct str) + b2w(((str) x)->len); }

static void wk_two(state v, word x, word *pool0, word *top0) {
  v->cp += Width(struct two);
  A(x) = cp(v, A(x), pool0, top0);
  B(x) = cp(v, B(x), pool0, top0); }

void transmit(state v, FILE* o, word x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (datp((verb) x)) gettyp(x)->emit(v, o, x);
  else fprintf(o, "#%lx", x); }

void tx_str(state v, FILE *o, word _) {
  str s = (str) _;
  size len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

void tx_two(state v, FILE *o, word x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

// internal parser functions
static word
  rx_ret(state, FILE*, word),
  rxr(state, FILE*),
  rx_two(state, FILE*),
  rx_atom(state, str);

// FIXME should distinguish between OOM and parse error
static status receive(state v, FILE *i) {
  word x; return
    !push1(v, (word) rx_ret) ? OomError :
    !(x = rxr(v, i)) ? feof(i) ? Eof : DomainError :
    push1(v, x) ? Ok : OomError; }

status receive2(state f, char *_i) {
  size len = strlen(_i);
  char *i = malloc(len + 1);
  if (!i) return OomError;
  memcpy(i, _i, len);
  i[len] = 0;
  FILE *in = fmemopen(i, len, "r");
  if (!in) return free(i), OomError;
  status s = receive(f, in);
  return fclose(in), free(i), s; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

static Inline word pull(state v, FILE *i, word x) { return
  ((word (*)(state, FILE*, word)) pop1(v))(v, i, x); }

// get the next token character from the stream
static NoInline int rx_char(FILE *i) {
  for (int c;;) switch (c = getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (getc(i)) {
      case '\n': case EOF: return rx_char(i); } } }

static word rx_ret(state v, FILE* i, word x) { return x; }

static word rx_two_cons(state v, FILE* i, word x) {
  word y = pop1(v); return
    x = x ? (ob) pair(v, y, x) : x,
    pull(v, i, x); }

static word rx_two_cont(state v, FILE* i, word x) { return
  !x || !push2(v, (word) rx_two_cons, x) ? pull(v, i, 0) : rx_two(v, i); }

static ob rx_q_cont(state f, FILE *i, word x) {
  str s;
  if (x && (x = (word) pair(f, x, nil)) && (x = (word) pair(f, nil, x)) && push1(f, x)) {
    str s = strof(f, Quote);
    x = pop1(f);
    if (!s) x = 0;
    else A(x) = (ob) s; }
  return pull(f, i, x); }

static str rx_atom_chars(state, FILE*), rx_str(state, FILE*);
static word rx_q(state, FILE*);

static NoInline word rxr(state v, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(v, i, 0);
    case '(': return rx_two(v, i);
    case '"': return pull(v, i, (word) rx_str(v, i));
    case '\'': return rx_q(v, i);
    default:
      ungetc(c, i);
      str a = rx_atom_chars(v, i);
      word x = a ? rx_atom(v, a) : 0;
      return pull(v, i, x); } }

static ob rx_q(state f, FILE *i) {
  return push1(f, (word) rx_q_cont) ? rxr(f, i) : pull(f, i, 0); }

static ob rx_two(li v, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(v, i, nil);
    default: return ungetc(c, i),
      push1(v, (word) rx_two_cont) ? rxr(v, i) : pull(v, i, 0); } }

static str buf_new(state f) {
  str s = cells(f, Width(struct str) + 1);
  return s ? str_ini(s, sizeof(ob)) : s; }

static NoInline str buf_grow(state f, str s) {
  str t; size_t len = s->len; return
    avec(f, s, t = cells(f, Width(struct str) + 2 * b2w(len))),
    !t ? t : (memcpy(t->text, s->text, len),
              str_ini(t, 2 * len)); }
  
// read the contents of a string literal into a string
static NoInline str rx_str(li v, FILE* p) {
  str o = buf_new(v);
  for (size_t n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = getc(p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }
// read the characters of an atom (number or symbol)
// into a string
static NoInline str rx_atom_chars(state v, FILE* p) {
  str o = buf_new(v);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      default: o->text[n++] = x; continue;
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': ungetc(x, p);
      case EOF: return o->len = n, o; }
  return 0; }

#include <ctype.h>
static NoInline word rx_atom_n(state v, str b, size inset, int sign, int rad) {
  static const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  size len = b->len;
  if (inset >= len) fail: return (ob) b;
  intptr_t out = 0;
  do {
    int dig = 0, c = tolower(b->text[inset++]);
    while (digits[dig] && digits[dig] != c) dig++;
    if (dig >= rad) goto fail;
    out = out * rad + dig;
  } while (inset < len);
  return putnum(sign * out); }

static NoInline word rx_atom(state v, str b) {
  intptr_t i = 0, len = b->len, sign = 1;
  while (i < len) switch (b->text[i]) {
    case '+': i += 1; continue;
    case '-': i += 1, sign *= -1; continue;
    case '0': if (i+1 < len) {
      const char *r = "b\2s\6o\10d\12z\14x\20n\44";
      for (char c = tolower(b->text[i+1]); *r; r += 2)
        if (*r == c) return rx_atom_n(v, b, i+2, sign, r[1]); }
    default: goto out; } out:
  return rx_atom_n(v, b, i, sign, 10); }

#define shew(s,x) (printf(s),transmit(f,stdout,x),puts(""))
// echo loop
static enum status go(state f) {
  printf("# dim=%ld f@0x%lx[len=%ld]\n", sizeof(word), (word) f, f->len);
#ifdef testing
  self_test(f);
  assert(f->sp == f->pool + f->len);
#endif
  // echo loop
  intptr_t s, height = f->pool + f->len - f->sp;
  while ((s = receive(f, stdin)) != Eof) {
    if (s == Ok && (s = eval(f, pop1(f))) == Ok)
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout),
      assert(f->sp == f->pool + f->len);
    else
      fprintf(stderr, "# status %ld\n", s),
      f->sp = f->pool + f->len - height; }
  return Ok; }

int main(int ac, char **av) {
  state f = &((struct carrier){});
  status s = li_ini(f);
  if (s == Ok) s = go(f), li_fin(f);
  return s; }
