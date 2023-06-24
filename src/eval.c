#include "i.h"

enum status l_evals(struct l_state *f, const char *prog) {
  enum status s = receive2(f, prog);
  return s != Ok ? s : eval(f, pop1(f)); }

typedef uintptr_t size;

// list length
static size_t llen(ob l) {
  size_t n = 0;
  while (twop(l)) n++, l = B(l);
  return n; }

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

static verb cata(state f, struct cctx **c, size_t m) {
  verb k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(word));
  return pull_thread(f, c, k + m); }

static ca ana, ana_list, ana_str, ana_ap;
NoInline status eval(state f, word x) {
  struct cctx *c = pushn(f, 2, x, (word) yield_thread) ? scope(f, NULL) : NULL;
  verb k = 0;
  if (c) {
    size m;
    avec(f, c,
      m = ana(f, &c, 1, pop1(f)),
      m = m && pushn(f, 2, (word) e1, (word) yield) ? m : 0,
      k = m ? cata(f, &c, m) : k); }
  return !k ? OomError : k->ap(f, k, f->hp, f->sp); }

static Ana(value) {
  return pushn(f, 3, (word) e2, (word) K, x) ? m + 2 : 0; }

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
  return pushn(f, 3, (word) em_var, x, (*c)->sn) ? m + 2 : 0; }

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
  if (!pushn(f, 2, x, (word) ana_cond_pop_continuation)) return 0;
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
    m = m && pushn(f, 1, (word) ana_cond_pop_alternative) ? m : 0;
    m = m ? ana(f, c, m + 2, A(B(x))): 0;
    m = m && pushn(f, 1, (word) ana_cond_push_alternative) ? m + 2 : 0; }
  UM(f);
  return m && pushn(f, 1, (word) ana_cond_push_continuation) ? m : 0; }

// reverse decons: pushes last list item to stack, returns init of list.
static word snoced(state f, word x) {
  if (!twop(x)) return pushn(f, 1, nil) ? nil : 0;
  if (!twop(B(x))) return pushn(f, 1, A(x)) ? nil : 0;
  ob y = A(x);
  avec(f, y, x = snoced(f, B(x)));
  return x ? (word) pair(f, y, x) : x; }

static Ana(ana_lambda) {
  if (!(x = snoced(f, x)) || !pushn(f, 1, x)) return 0;
  struct cctx *d = scope(f, c);
  if (!d) return 0;
  d->sb = pop1(f);
  MM(f, &d);
  size inner_m = pushn(f, 2, pop1(f), (word) yield_thread) ? ana(f, &d, 4, pop1(f)) : 0;
  if (inner_m) {
    size sbn = llen(d->sb);
    verb k = pushn(f, 3, (word) e2, (word) ret, putnum(sbn)) ? cata(f, &d, inner_m) : 0;
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
    m = m && pushn(f, 1, (word) em_ap) ? m : 0;
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

intptr_t liprng(intptr_t n) {
  const intptr_t steele_vigna_2021 = 0xaf251af3b0f025b5;
  return (1 + n * steele_vigna_2021) >> sizeof(intptr_t); }

void l_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

enum status l_ini(struct l_state *f) {
  memset(f, 0, sizeof(struct l_state));
  const size_t len0 = 1; // a power of 2
  ob *pool = malloc(len0 * 2 * sizeof(intptr_t));
  if (!pool) return OomError;
  f->loop = f->sp = (f->pool = f->hp = pool) + (f->len = len0);
  f->t0 = clock();
  return Ok; }

static verb mo_ini(void *_, size len) {
  struct tag *t = (struct tag*) ((verb) _ + len);
  return t->null = NULL, t->head = _; }
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
