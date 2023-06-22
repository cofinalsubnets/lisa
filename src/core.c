#include "i.h"
#include <stdio.h>
#include <errno.h>
static status
  apply(state, verb, word*, word*),
  K(state, verb, word*, word*),
  curry(state, verb, word*, word*),
  retn(state, verb, word*, word*);

struct str {
  status (*act)(state, verb, word*, word*);
  struct methods *typ;
  uintptr_t len;
  char text[]; };
#define Cond "?"
#define Lambda "\\"
static bool eql(state, word, word), please(state, size),
            eq_two(O, ob, ob), eq_str(O, ob, ob);
#define avail(f) (f->sp-f->hp)
#define Width(_) b2w(sizeof(_))

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
  void *x = f->hp; f->hp += n; return x; }

static void *cells(state f, size n) {
  return avail(f) < n && !please(f, n) ? 0 : bump(f, n); }

static status gc(state, verb, word*, word*, size);
static void wk_two(O, ob, ob*, ob*),
     wk_str(O, ob, ob*, ob*),
     tx_two(O, FILE*, ob),
     tx_str(O, FILE*, ob);
static ob cp_two(O, ob, ob*, ob*),
   cp_str(O, ob, ob*, ob*);

// push things onto the stack
static NoInline bool pushsr(state f, size i, va_list xs) {
  bool _; word x = va_arg(xs, word);
  if (!x) return avail(f) >= i || please(f, i);
  avec(f, x, _ = pushsr(f, i + 1, xs));
  if (!_) return _;
  *--f->sp = x;
  return true; }

static NoInline bool pushs(state f, ...) {
  bool _; va_list xs;
  va_start(xs, f), _ = pushsr(f, 0, xs), va_end(xs);
  return _; }

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

static NoInline verb thdr(state f, size n, va_list xs) {
  word x = va_arg(xs, ob);
  if (!x) return mo_n(f, n);
  verb k; avec(f, x, k = thdr(f, n + 1, xs));
  if (k) k[n].x = x;
  return k; }

NoInline verb thd(state f, ...) {
  verb k; va_list xs; return
    va_start(xs, f),
    k = thdr(f, 0, xs),
    va_end(xs),
    k; }

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
  return (void*) k; }

static verb yield_thread(state f, struct cctx **c, verb k) {
  struct tag *t = mo_tag(k);
  assert(t->head <= k);
  return t->head = k; }

static verb pull_thread(state f, struct cctx **c, verb k) { return
  ((verb (*)(state, struct cctx**, verb)) (*f->sp++))(f, c, k); }

static status recn(state f, verb ip, word *hp, word *sp) {
  word x = sp[0], j = sp[1];
  sp += getnum(ip[1].x) + 1;
  if (nump(j)) ip = (verb) *++sp, *sp = j;
  else ip = (verb) j, *sp = x;
  return ip->ap(f, ip, hp, sp); }

static verb apf(state f, struct cctx **c, verb k) {
  if (k->ap == retn) k->ap = recn;
  else (--k)->ap = apply;
  return pull_thread(f, c, k); }

static verb e1(state f, struct cctx **c, verb k) {
  return (--k)->x = *f->sp++, pull_thread(f, c, k); }

static verb e2(state f, struct cctx **c, verb k) {
  return k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static word lidx(state f, ob l, ob x) {
  for (word i = 0; twop(l); l = B(l), i++) if (eql(f, A(l), x)) return i;
  return -1; }

#define Pack() (f->ip = ip, f->hp = hp, f->sp = sp)
#define Have(n) if (sp - hp < n) return gc(f, ip, hp, sp, n)
#define Have1() if (sp == hp) return gc(f, ip, hp, sp, 1)

static status ref(state f, verb ip, word *hp, word *sp) {
  Have1();
  word x = sp[getnum(ip[1].x)];
  *--sp = x;
  return ip += 2, ip->ap(f, ip, hp, sp); }

static verb var(state f, struct cctx **c, verb k) {
  word sym = *f->sp++,
       idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  return (--k)->x = putnum(idx),
         (--k)->ap = ref,
         pull_thread(f, c, k); }

static size ana(state, struct cctx**, size, word);
static verb cata(O f, struct cctx **c, size m) {
  assert((*c)->sn == nil);
  verb k = mo_n(f, m);
  return !k ? k : pull_thread(f, c, (verb) memset(k, -1, m * sizeof(word)) + m); }

status eval(state f, word x) {
  size m; verb k = 0;
  struct cctx *c = pushs(f, x, yield_thread, End) ? scope(f, NULL) : NULL;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m && pushs(f, e1, yield, End) ? m : 0,
    k = m ? cata(f, &c, m) : k);
  return !k ? OomError : (f->ip = k, li_go(f)); }


bool kstrq(str s0, const char *s1) { return
  strlen(s1) == s0->len &&
  0 == strncmp(s0->text, s1, s0->len); }

static size value(state f, struct cctx**c, size m, word x) {
  return pushs(f, e2, K, x, End) ? m + 2 : 0; }

static size ana_str(state f, struct cctx **c, size m, word x) {
  if (nilp((word) (*c)->par)) return value(f, c, m, x);
  if (lidx(f, (*c)->sb, x) < 0) {
    x = (word) pair(f, x, (*c)->sb);
    if (!x) return x;
    (*c)->sb = x;
    x = (word) pair(f, A(x), (*c)->ib);
    if (!x) return x;
    (*c)->ib = x;
    x = A(x); }
  return pushs(f, var, x, (*c)->sn, End) ? m + 2 : 0; }

static size ana_list(state, struct cctx**, size, word);
static size ana(state f, struct cctx **c, size m, word x) {
  return twop(x) ? ana_list(f, c, m, x) :
         strp(x) ? ana_str(f, c, m, x) :
                   value(f, c, m, x); }

static verb
  ana_cond_push_continuation(state, struct cctx **, verb),
  ana_cond_push_alternative(state, struct cctx**, verb),
  ana_cond_pop_alternative(state, struct cctx**, verb),
  ana_cond_pop_continuation(state, struct cctx**, verb);

static status branch(state f, verb ip, word *hp, word *sp) {
  ip = nilp(*sp++) ? ip[1].m : ip + 2;
  return ip->ap(f, ip, hp, sp); }

static verb ana_cond_pop_alternative(state f, struct cctx**c, verb k) {
  (--k)->x = A((*c)->s1);
  (--k)->ap = branch;
  (*c)->s1 = B((*c)->s1);
  return pull_thread(f, c, k); }

static size ana_cond(state f, struct cctx **c, uintptr_t m, word x) {
  if (!pushs(f, x, ana_cond_pop_continuation, End)) return 0;
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
    m = m && pushs(f, ana_cond_pop_alternative, End) ? m : 0;
    m = m ? ana(f, c, m + 2, A(B(x))): 0;
    m = m && pushs(f, ana_cond_push_alternative, End) ? m + 2 : 0; }
  UM(f);
  return m && pushs(f, ana_cond_push_continuation, End) ? m : 0; }

// reverse decons: pushes last list item to stack, returns init of list.
static word snoced(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  ob y = A(x);
  avec(f, y, x = snoced(f, B(x)));
  return x ? (word) pair(f, y, x) : x; }

static size ana_lambda(state f, struct cctx **c, size m, word x) {
  if (!(x = snoced(f, x)) || !push1(f, x)) return 0;
  struct cctx *d = scope(f, c);
  if (!d) return 0;
  d->sb = pop1(f);
  size l, n; verb k;
  avec(f, d,
    n = pushs(f, pop1(f), yield_thread, End) ? ana(f, &d, 4, pop1(f)) : 0,
    k = n && pushs(f, e2, retn, putnum(l = llen(d->sb)), End) ? cata(f, &d, n) : 0,
    k = k && l > 1 ? (k[-2].ap = curry, k[-1].x = putnum(l), k - 2) : k,
    x = k && twop(d->ib) ? (word) pair(f, (word) k, d->ib) : (word) k);
  return x ? ana(f, c, m, x) : x; }

static size ana_quote(state f, struct cctx **c, size m, word x) {
  return value(f, c, m, twop(x) ? A(x) : x); }

static size (*special_form(str s))(state, struct cctx**, size, word) {
  return kstrq(s, Quote)  ? ana_quote :
         kstrq(s, Cond)   ? ana_cond :
         kstrq(s, Lambda) ? ana_lambda :
                            NULL; }

static size ana_list(state f, struct cctx **c, size m, word x) {
  size (*form)(state, struct cctx**, size, word);
  if (strp(A(x)) && (form = special_form((str) A(x))))
    return form(f, c, m, B(x));
  MM(f, &x);
  m = ana(f, c, m, A(x));
  (*c)->sn += 2;
  while (m && twop(x = B(x)))
    m = ana(f, c, m, A(x)),
    m = m && pushs(f, apf, End) ? m + 1 : 0;
  return (*c)->sn -= 2, UM(f), m; }

static verb ana_cond_push_continuation(state f, struct cctx **c, verb k) {
  two w = pair(f, (word) k, (*c)->s2);
  return !w ? (verb) w :
    pull_thread(f, c, (verb) A((*c)->s2 = (word) w)); }

static status jump(state f, verb ip, word *hp, word *sp) {
  return ip = ip[1].m,
         ip->ap(f, ip, hp, sp); }

static verb ana_cond_push_alternative(state f, struct cctx**c, verb k) {
  two w = pair(f, (ob) k, (*c)->s1);
  if (!w) return (verb) w;
  k = (verb) A((*c)->s1 = (ob) w);
  verb kk = (verb) A((*c)->s2);
  if (kk->ap != retn) k[-1].m = kk, k[-2].ap = jump;
  else k[-1].x = kk[1].x, k[-2].ap = retn;
  return pull_thread(f, c, k - 2); }

static verb ana_cond_pop_continuation(state f, struct cctx **c, verb k) {
  return (*c)->s2 = B((*c)->s2),
         pull_thread(f, c, k); }

status yield(state f, verb ip, word *hp, word *sp) {
  return Pack(), Ok; }

status K(state f, verb ip, word *hp, word *sp) {
  Have1();
  *--sp = ip[1].x;
  ip += 2;
  return ip->ap(f, ip, hp, sp); }

static status Kj(state f, verb ip, word *hp, word *sp) {
  Have1();
  *--sp = ip[1].x;
  ip = ip[2].m;
  return ip->ap(f, ip, hp, sp); }

status retn(state f, verb ip, word *hp, word *sp) {
  word r = *sp; return sp += getnum(ip[1].x) + 1,
                       ip = (verb) *sp,
                       *sp = r,
                       ip->ap(f, ip, hp, sp); }

status apply(state f, verb ip, word *hp, word *sp) {
  if (nump(sp[1])) return ip[1].ap(f, ip + 1, hp, sp + 1);
  verb k = (verb) sp[1];
  sp[1] = (word) (ip + 1);
  return k->ap(f, k, hp, sp); }

static status curry(state f, verb ip, word *hp, word *sp) {
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

status data(state f, verb ip, ob *hp, ob *sp) {
  word x = (word) ip;
  ip = (verb) *++sp;
  *sp = x;
  return ip->ap(f, ip, hp, sp); }

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
#define big (want < need || vim < vim_inf)
#define little (want >> 1 > need && vim > vim_sup)
static void copy_from(state, word*, word*);
static NoInline bool please(state v, size req) {
  size t1 = clock(), t0 = v->t0, have = v->len;
  word *pool = v->pool, *loop = v->loop;
  v->pool = loop, v->loop = pool;
  copy_from(v, pool, pool + have);
  size t2 = v->t0 = clock(),
       vim = t2 == t1 ? vim_sup : (t2 - t0) / (t2 - t1),
       want = have,
       need = have - (avail(v) - req);
  if         (big) do want <<= 1, vim <<= 1; while (big);
  else if (little) do want >>= 1, vim >>= 1; while (little);
  else return true; // else no resize is needed, so return success
  // try and resize
  //
  word *new = malloc(want * 2 * sizeof(word)); // allocate a new pool
  if (!new) return need <= have; // if that fails, succeed iff the first copy is big enough
  // we got a new pool; copy again, free the old pool, return ok
  v->loop = (v->pool = new) + (v->len = want);
  copy_from(v, loop, loop + have);
  free(pool < loop ? pool : loop);
  v->t0 = clock();
  return true; }

static word cp(state, word, word*, word*);
static NoInline void copy_from(state v, word *pool0, word *top0) {
  size len1 = v->len;
  word *sp0 = v->sp,
       *pool1 = v->pool,
       *top1 = pool1 + len1;
  size slen = top0 - sp0;
  // reset heap
  v->hp = v->cp = pool1;
  // copy stack
  word *sp1 = v->sp = top1 - slen;
  for (size i = 0; i < slen; i++)
    sp1[i] = cp(v, sp0[i], pool0, top0);
  // copy registers
  v->ip = (verb) cp(v, (word) v->ip, pool0, top0);
  // copy user values
  for (struct ll *r = v->safe; r; r = r->next)
    *r->addr = cp(v, *r->addr, pool0, top0);
  // cheney's algorithm
  for (mo k; (k = (mo) v->cp) < (mo) v->hp;)
    if (datp(k)) gettyp(k)->walk(v, (ob) k, pool0, top0);
    else { for (; k->ap; k++) k->x = cp(v, k->x, pool0, top0);
           v->cp = (ob*) k + 2; } }

static NoInline word cp_mo(state v, verb src, word *pool0, word *top0) {
  struct tag *fin = mo_tag(src);
  verb ini = fin->head,
       dst = bump(v, fin->end - ini),
       d = dst;
  for (verb s = ini; (d->x = s->x); s++->x = (ob) d++);
  return (d+1)->ap = (void*) dst,
         (ob) (src - ini + dst); }

// this can give a false positive if x is a fixnum
static Inline bool livep(state v, word x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

static NoInline word cp(state v, word x, word *pool0, word *top0) {
  if (nump(x) || (ob*) x < pool0 || (ob*) x >= top0) return x;
  verb src = (verb) x;
  if (homp(src->x) && livep(v, src->x)) return src->x;
  if (datp(src)) return gettyp(src)->evac(v, (word) src, pool0, top0);
  return cp_mo(v, src, pool0, top0); }

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
static word rx_ret(state, FILE*, word), rxr(state, FILE*),
            rx_two(state, FILE*), rx_atom(state, str);

// FIXME should distinguish between OOM and parse error
static status receive(state v, FILE *i) {
  word x; return
    !pushs(v, rx_ret, End) ? OomError :
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
  !x || !pushs(v, rx_two_cons, x, End) ? pull(v, i, 0) :
                                         rx_two(v, i); }

static str rx_atom_chars(state, FILE*), rx_str(state, FILE*);

static NoInline word rxr(state v, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(v, i, 0);
    case '(': return rx_two(v, i);
    case '"': return pull(v, i, (ob) rx_str(v, i));
    default:
      ungetc(c, i);
      str a = rx_atom_chars(v, i);
      ob x = a ? rx_atom(v, a) : 0;
      return pull(v, i, x); } }

static NoInline ob rx_two(li v, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(v, i, nil);
    default: return ungetc(c, i),
      pushs(v, rx_two_cont, End) ? rxr(v, i) :
                                   pull(v, i, 0); } }

static str buf_new(li v) {
  str s = cells(v, Width(struct str) + 1);
  return s ? str_ini(s, sizeof(ob)) : s; }

static NoInline str buf_grow(li v, str s) {
  str t; size_t len = s->len; return
    avec(v, s, t = cells(v, Width(struct str) + 2 * b2w(len))),
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

// echo loop
static enum status go(state f) {
  printf("# dim=%ld f@0x%lx[len=%ld]\n", sizeof(word), (word) f, f->len);
#ifdef testing
  self_test(f);
  assert(f->sp == f->pool + f->len);
#endif
  // echo loop
  intptr_t s, height = f->pool + f->len - f->sp;
  while ((s = receive(f, stdin)) != Eof)
    if (s == Ok && (s = eval(f, pop1(f))) == Ok)
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout),
      assert(f->sp == f->pool + f->len);
    else
      fprintf(stderr, "# status %ld\n", s),
      f->sp = f->pool + f->len - height;
  return Ok; }

int main(int ac, char **av) {
  state f = &((struct carrier){});
  status s = li_ini(f);
  if (s == Ok) s = go(f), li_fin(f);
  return s; }
