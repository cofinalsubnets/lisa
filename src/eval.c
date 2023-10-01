#include "i.h"

static struct scope {
  word s1, s2, ib, sb, sn;
  struct scope *par;
} *scope(state f, struct scope **par, word sb) {
  struct scope *sc = (struct scope *) mo_n(f, Width(struct scope));
  if (sc) sc->sb = sb,
          sc->s1 = sc->s2 = sc->ib = sc->sn = nil,
          sc->par = par ? *par : (struct scope*) nil;
  return sc; }

#define Ana(n) size_t n(state f, struct scope**c, size_t m, word x)
#define Cata(n) cell n(state f, struct scope **c, cell k)
typedef Ana(ana_);
typedef Cata(cata_);

static Cata(yield_thread) { return k; }
static Cata(pull_thread) { return ((cata_*) (*f->sp++))(f, c, k); }
static Cata(cata_ret) { return k[-2].ap = ret, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }
static Cata(cata_val) { return k[-2].ap = K, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static Cata(cata_yield) { return k[-1].ap = yield, pull_thread(f, c, k - 1); }
static Cata(cata_ap) {
  if (k->ap == ret) k->ap = tap;
  else (--k)->ap = ap;
  return pull_thread(f, c, k); }

static Cata(cata_var) {
  word sym = *f->sp++,
       idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  k[-2].ap = ref;
  k[-1].x = putnum(idx);
  return pull_thread(f, c, k - 2); }

static cell cata(state f, struct scope **c, size_t m) {
  cell k = mo_n(f, m);
  if (!k) return k;
  memset(k, -1, m * sizeof(word));
  return pull_thread(f, c, k + m); }

static ana_ ana, ana_list, ana_str, ana_ap;
NoInline enum status eval(state f, word x) {
  size_t m;
  cell k = 0;
  struct scope *c =
    push2(f, x, (word) yield_thread) ?
      scope(f, NULL, nil) :
      NULL;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m && push1(f, (word) cata_yield) ? m : 0,
    k = m ? cata(f, &c, m) : k);
  return !k ? Oom :
    k->ap(f, k, f->hp, f->sp); }

static Ana(value) {
  return push2(f, (word) cata_val, x) ? m + 2 : 0; }
static Ana(ana) {
  if (homp(x) && datp(x)) switch (ptr(x)[1].x) {
    case Pair: return ana_list(f, c, m, x);
    case String: return ana_str(f, c, m, x); }
  return value(f, c, m, x); }


static bool inboundp(state f, struct scope *c, word y) {
  return lidx(f, c->sb, y) >= 0; }

static bool outboundp(state f, struct scope *c, word y) {
  for (c = c->par; !nilp((word) c); c = c->par)
    if (lidx(f, c->sb, y) >= 0) return true;
  return false; }

static Ana(ana_str) {
  if (!inboundp(f, *c, x)) {
    if (!outboundp(f, *c, x)) {
      word y = dict_assoc(f, x);
      // self-quoting if undefined at top level
      return value(f, c, m, y ? y : x); }
    else {
      x = (word) cons(f, x, (*c)->sb);
      if (x) (*c)->sb = x, x = (word) cons(f, A(x), (*c)->ib);
      if (x) (*c)->ib = x, x = A(x); } }
  return x &&
    push2(f, x, (*c)->sn) &&
    push1(f, (word) cata_var) ? m + 2 : 0; }

static Cata(cata_cond_post_branch) {
  k[-2].ap = cond;
  k[-1].x = A((*c)->s1);
  (*c)->s1 = B((*c)->s1);
  return pull_thread(f, c, k - 2); }

static Cata(cata_cond_pre_emit) {
  two w = cons(f, (word) k, (*c)->s2);
  if (!w) return (thread) w;
  (*c)->s2 = (word) w;
  return pull_thread(f, c, (thread) A(w)); }

static Cata(cata_cond_pre_branch) {
  two w = cons(f, (word) k, (*c)->s1);
  if (!w) return (thread) w;
  (*c)->s1 = (word) w;
  k = (thread) A(w) - 2;
  cell kk = (cell) A((*c)->s2);
  // if the destination is a return or tail call,
  // then forward it instead of emitting a jump.
  if (kk->ap == ret || kk->ap == tap)
    k[0].ap = kk->ap,
    k[1].x = kk[1].x;
  else
    k[0].ap = jump,
    k[1].m = kk;
  return pull_thread(f, c, k); }

static Cata(cata_cond_post_emit) {
  (*c)->s2 = B((*c)->s2);
  return pull_thread(f, c, k); }

static Ana(ana_cond) {
  if (!push2(f, x, (word) cata_cond_post_emit)) return 0;
  for (x = pop1(f), MM(f, &x); m; x = B(B(x))) {
    if (!twop(x) && !(x = (word) cons(f, x, nil))) {
      m = 0;
      break; }
    if (!twop(B(x))) {
      m = ana(f, c, m, A(x));
      break; }
    m = ana(f, c, m + 4, A(x));
    m = m && push1(f, (word) cata_cond_post_branch) ? m : 0;
    m = m ? ana(f, c, m, A(B(x))) : 0;
    m = m && push1(f, (word) cata_cond_pre_branch) ? m : 0; }
  return UM(f), m && push1(f, (word) cata_cond_pre_emit) ? m : 0; }

// lambda decons: pushes last list item to stack, returns init of list.
static word ldecons(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  word y = A(x);
  avec(f, y, x = ldecons(f, B(x)));
  return x ? (word) cons(f, y, x) : x; }

static Ana(ana_lambda) {
  if (!(x = ldecons(f, x))) return 0;
  struct scope *d = scope(f, c, x);
  if (!d) return 0;
  MM(f, &d);
  size_t m_in =
    push2(f, pop1(f), (word) yield_thread) ?
      ana(f, &d, 4, pop1(f)) : 0;
  if (m_in) {
    size_t sbn = llen(d->sb);
    thread k =
      push2(f, (word) cata_ret, putnum(sbn)) ?
        cata(f, &d, m_in) : 0;
    if (k) {
      if (sbn > 1)
        (--k)->x = putnum(sbn),
        (--k)->ap = curry;
      mo_tag(k)->head = k; }
    x = k && twop(d->ib) ? (word) cons(f, (word) k, d->ib) : (word) k; }
  UM(f);
  return x ? ana(f, c, m, x) : x; }

static Ana(ana_list) {
  if (!twop(B(x))) // singleton lists quote
    return value(f, c, m, A(x));
  if (strp(A(x))) {
    string s = (string) A(x);
    if (s->len == 1) switch (s->text[0]) {
      case '?': return ana_cond(f, c, m, B(x));
      case '\\': return ana_lambda(f, c, m, B(x)); } }
  return ana_ap(f, c, m, x); }


static Ana(ana_ap) {
  MM(f, &x);
  m = ana(f, c, m, A(x));
  (*c)->sn += 2;
  while (m && twop(x = B(x)))
    m = ana(f, c, m + 1, A(x)),
    m = m && push1(f, (word) cata_ap) ? m : 0;
  (*c)->sn -= 2;
  UM(f);
  return m; }
