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
#define Cata(n) verb n(state f, struct scope **c, verb k)
typedef Ana(ca); typedef Cata(cc);

static Cata(yield_thread) { return k; }
static Cata(pull_thread) { return ((cc*) (*f->sp++))(f, c, k); }
static Cata(cata_ret) { return k[-2].ap = ret, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }
static Cata(cata_val) { return k[-2].ap = K, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static Cata(cata_yield) { return k[-1].ap = yield, pull_thread(f, c, k - 1); }
static Cata(cata_ap) {
  if (k->ap == ret) k->ap = rec;
  else (--k)->ap = ap;
  return pull_thread(f, c, k); }

static Cata(cata_var) {
  word sym = *f->sp++, idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  return k[-2].ap = var, k[-1].x = putnum(idx), pull_thread(f, c, k - 2); }

static verb cata(state f, struct scope **c, size_t m) {
  verb k = mo_n(f, m); return !k ? k :
    (memset(k, -1, m * sizeof(word)), pull_thread(f, c, k + m)); }

static ca ana, ana_list, ana_str, ana_ap;
NoInline enum status eval(state f, word x) {
  struct scope *c = push2(f, x, (word) yield_thread) ? scope(f, NULL, nil) : NULL;
  size_t m; verb k = 0;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m && push1(f, (word) cata_yield) ? m : 0,
    k = m ? cata(f, &c, m) : k);
  return !k ? Oom : k->ap(f, k, f->hp, f->sp); }

static Ana(value) { return push2(f, (word) cata_val, x) ? m + 2 : 0; }
static Ana(ana) { return twop(x) ? ana_list(f, c, m, x) :
                         strp(x) ? ana_str(f, c, m, x) :
                                   value(f, c, m, x); }

static Ana(ana_str) {
  if (nilp((word) (*c)->par)) return value(f, c, m, x);
  if (lidx(f, (*c)->sb, x) < 0) {
    x = (word) cons(f, x, (*c)->sb);
    if (x) (*c)->sb = x, x = (word) cons(f, A(x), (*c)->ib);
    if (x) (*c)->ib = x, x = A(x); }
  return x && push2(f, x, (*c)->sn) && push1(f, (word) cata_var) ? m + 2 : 0; }

static Cata(cata_cond_pop_a) { return
  k[-2].ap = br,
  k[-1].x = A((*c)->s1),
  (*c)->s1 = B((*c)->s1),
  pull_thread(f, c, k - 2); }

static Cata(cata_cond_push_c) {
  two w = cons(f, (word) k, (*c)->s2);
  return !w ? (verb) w : pull_thread(f, c, (verb) A((*c)->s2 = (word) w)); }

static Cata(cata_cond_push_a) {
  two w = cons(f, (word) k, (*c)->s1);
  if (!w) return (verb) w;
  k = (verb) A((*c)->s1 = (word) w) - 2;
  verb kk = (verb) A((*c)->s2);
  // if the destination is a return or tail call,
  // then forward it instead of emitting a jump.
  if (kk->ap == ret || kk->ap == rec)
    k[0].ap = kk->ap, k[1].x = kk[1].x;
  else k[0].ap = jump, k[1].m = kk;
  return pull_thread(f, c, k); }

static Cata(cata_cond_pop_c) {
  return (*c)->s2 = B((*c)->s2), pull_thread(f, c, k); }

static Ana(ana_cond) {
  if (!push2(f, x, (word) cata_cond_pop_c)) return 0;
  for (x = pop1(f), MM(f, &x); m; x = B(B(x))) {
    if (!twop(x) && !(x = (word) cons(f, x, nil))) { m = 0; break; }
    if (!twop(B(x))) { m = ana(f, c, m, A(x)); break; }
    m = ana(f, c, m + 4, A(x));
    m = m && push1(f, (word) cata_cond_pop_a) ? m : 0;
    m = m ? ana(f, c, m, A(B(x))) : 0;
    m = m && push1(f, (word) cata_cond_push_a) ? m : 0; }
  return UM(f), m && push1(f, (word) cata_cond_push_c) ? m : 0; }

// reverse decons: pushes last list item to stack, returns init of list.
static word snoced(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  word y = A(x); return avec(f, y, x = snoced(f, B(x))),
                        x ? (word) cons(f, y, x) : x; }

static Ana(ana_lambda) {
  if (!(x = snoced(f, x))) return 0;
  struct scope *d = scope(f, c, x);
  if (!d) return 0;
  MM(f, &d);
  size_t m_in = push2(f, pop1(f), (word) yield_thread) ? ana(f, &d, 4, pop1(f)) : 0;
  if (m_in) {
    size_t sbn = llen(d->sb);
    verb k = push2(f, (word) cata_ret, putnum(sbn)) ? cata(f, &d, m_in) : 0;
    if (k) {
      if (sbn > 1) k -= 2, k[0].ap = cur, k[1].x = putnum(sbn);
      mo_tag(k)->head = k; }
    x = k && twop(d->ib) ? (word) cons(f, (word) k, d->ib) : (word) k; }
  UM(f);
  return x ? ana(f, c, m, x) : x; }

static Ana(ana_list) {
  if (!twop(B(x))) return value(f, c, m, A(x)); // singletons quote
  if (strp(A(x))) {
    str s = (str) A(x);
    if (s->len == 1) switch (s->text[0]) {
      case '`': return x = B(x), value(f, c, m, twop(x) ? A(x) : x);
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
