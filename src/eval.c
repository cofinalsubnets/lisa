#include "i.h"

static thread compile(state, word);
// takes an expression compiles and executes it
NoInline status eval(state f, word x) {
  thread k = compile(f, x);
  return k ? k->ap(f, k, f->hp, f->sp) : Oom; }

// tracks lexical scope
typedef struct scope {
  word ib, // internal bindings
       sb, // stack bindings
       sn, // stack inset
       s1, s2; // stacks used for branch addresses
  struct scope *par;
} *scope;

// the compiler works by analyzing expressions and
// computing an upper bound on the thread length
// while pushing CPS code-emitting "pullback" functions
// onto the stack (ana phase)
#define Ana(n, ...)\
  size_t n(state f, scope *c, size_t m, word x, ##__VA_ARGS__)
typedef Ana(ana_);
// when the analysis is finished the
// thread is allocated and code is emitted as pullbacks
// are applied (cata phase)
#define Cata(n, ...)\
  cell n(state f, scope *c, cell k, ##__VA_ARGS__)
typedef Cata(cata_);

static ana_ ana, ana_list, ana_variable, ana_ap;
static cata_ yield_thread, pull_thread, cata_yield, cata_val, cata_ret, cata_variable;

static Cata(yield_thread) { return k; }
static Cata(pull_thread) { return ((cata_*) (*f->sp++))(f, c, k); }

static Cata(cata_yield) {
  k[-1].ap = yield;
  return pull_thread(f, c, k - 1); }

static Cata(cata_ret) {
  k[-2].ap = ret;
  k[-1].x = *f->sp++;
  return pull_thread(f, c, k - 2); }

static Cata(cata_val) {
  k[-2].ap = K;
  k[-1].x = *f->sp++;
  return pull_thread(f, c, k - 2); }

static Cata(cata_ap) {
  if (k->ap == ret) k->ap = tap;
  else (--k)->ap = ap;
  return pull_thread(f, c, k); }

static struct scope *enscope(state f, struct scope **par, word sb) {
  scope sc = (scope) mo_n(f, Width(struct scope));
  if (sc) sc->sb = sb,
          sc->s1 = sc->s2 = sc->ib = sc->sn = nil,
          sc->par = par ? *par : (scope) nil;
  return sc; }

static cell cata(state f, scope *c, size_t m) {
  cell k = mo_n(f, m);
  if (k) memset(k, -1, m * sizeof(word)), k += m;
  return pull_thread(f, c, k); }

static thread compile(state f, word x) {
  size_t m;
  cell k = 0;
  scope c = push2(f, x, (word) yield_thread) ?
    enscope(f, NULL, nil) : NULL;
  if (c) avec(f, c,
    m = ana(f, &c, 1, pop1(f)),
    m = m && push1(f, (word) cata_yield) ? m : 0,
    k = m ? cata(f, &c, m) : k);
  return k; }

static Ana(value) { return
  push2(f, (word) cata_val, x) ? m + 2 : 0; }

static Ana(ana) {
  if (homp(x) && datp(x)) switch (ptr(x)[1].x) {
    case Pair: return ana_list(f, c, m, x);
    case String: return ana_variable(f, c, m, x); }
  return value(f, c, m, x); }

static bool inboundp(state f, scope c, word y) {
  return lidx(f, c->sb, y) >= 0; }

static bool outboundp(state f, scope c, word y) {
  for (c = c->par; !nilp((word) c); c = c->par)
    if (lidx(f, c->sb, y) >= 0) return true;
  return false; }

static Ana(ana_variable) {
  if (!inboundp(f, *c, x)) {
    if (!outboundp(f, *c, x)) {
      word y = dict_lookup(f, x);
      // self-quoting if undefined at top level
      return value(f, c, m, y ? y : x); }
    else {
      x = (word) cons(f, x, (*c)->sb);
      if (x) (*c)->sb = x, x = (word) cons(f, A(x), (*c)->ib);
      if (x) (*c)->ib = x, x = A(x); } }
  return x &&
    push2(f, x, (*c)->sn) &&
    push1(f, (word) cata_variable) ? m + 2 : 0; }

static Cata(cata_variable) {
  word sym = *f->sp++,
       idx = getnum(*f->sp++) + lidx(f, (*c)->sb, sym);
  k[-2].ap = ref;
  k[-1].x = putnum(idx);
  return pull_thread(f, c, k - 2); }

static cata_
  cata_cond_post_emit,
  cata_cond_post_branch,
  cata_cond_pre_branch,
  cata_cond_pre_emit;

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


static void cata_cond_emit_jump(thread src, thread dst) {
  // if the destination is a return or tail call,
  // then forward it instead of emitting a jump.
  if (dst->ap == ret || dst->ap == tap)
    src[0].ap = dst[0].ap, src[1].x = dst[1].x;
  else src[0].ap = jump, src[1].x = (word) dst; }

static Cata(cata_cond_pre_branch) {
  pair w = cons(f, (word) k, (*c)->s1);
  if (!w) return (thread) w;
  (*c)->s1 = (word) w;
  k = (thread) A(w) - 2;
  cell target = (cell) A((*c)->s2);
  cata_cond_emit_jump(k, target);
  return pull_thread(f, c, k); }

static Cata(cata_cond_post_emit) {
  (*c)->s2 = B((*c)->s2);
  return pull_thread(f, c, k); }

// lambda decons: pushes last list item to stack, returns init of list.
static word ldecons(state f, word x) {
  if (!twop(x)) return push1(f, nil) ? nil : 0;
  if (!twop(B(x))) return push1(f, A(x)) ? nil : 0;
  word y = A(x);
  avec(f, y, x = ldecons(f, B(x)));
  return x ? (word) cons(f, y, x) : x; }


static word wrap_lambda(state f, scope d, size sbn, thread k) {
  if (!k) return (word) k;
  if (sbn > 1) // curry if more than 1 argument
    (--k)->x = putnum(sbn),
    (--k)->ap = curry;
  // trim thread
  mo_tag(k)->head = k;
  // apply to enclosed variables
  return (word) cons(f, (word) k, d->ib); }

static Ana(ana_lambda) {
  scope d; size sbn;
  if (!(x = ldecons(f, x)) ||
      !(d = enscope(f, c, x))) return 0;
  avec(f, d,
    x = push2(f, pop1(f), (word) yield_thread) ?
      ana(f, &d, 4, pop1(f)) : 0,
    x = !x ? x :
      (sbn = llen(d->sb),
       wrap_lambda(f, d, sbn,
         push2(f, (word) cata_ret, putnum(sbn)) ?
           cata(f, &d, x) : 0)));
  return x ? ana(f, c, m, x) : x; }

static Ana(ana_list) {
  // singleton list is quote
  if (!twop(B(x))) return value(f, c, m, A(x));
  // special form?
  if (strp(A(x))) {
    string s = (string) A(x);
    if (s->len == 1) switch (s->text[0]) {
      case '?': return ana_cond(f, c, m, B(x));
      case '\\': return ana_lambda(f, c, m, B(x)); } }
  // apply function to arguments
  MM(f, &x);
  m = ana(f, c, m, A(x));
  (*c)->sn += 2;
  while (m && twop(x = B(x)))
    m = ana(f, c, m + 1, A(x)),
    m = m && push1(f, (word) cata_ap) ? m : 0;
  (*c)->sn -= 2;
  UM(f);
  return m; }
