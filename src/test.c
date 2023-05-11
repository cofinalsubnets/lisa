#include "i.h"

#define Quote "`"
#define Cond "?"
#define Lambda "\\"
static verb compile_expression(state f, word x);
static bool kstrq(str, const char*);

static void test_big_list(state f) {
  size n = 1 << 20;
  for (size i = 0; i < n; i++)
    assert(push1(f, nil));
  for (ob l = nil; n--;)
    assert(l = (ob) pair(f, pop1(f), l)),
    assert(l = (ob) pair(f, l, l)); }

static status yield(state f, verb ip, word *hp, word *sp) {
  return Pack(), Ok; }

static status K(state f, verb ip, word *hp, word *sp) {
  Have1();
  return
    *--sp = ip[1].x,
    ip += 2,
    ip->ap(f, ip, hp, sp); }

static status show1(state f, verb ip, word *hp, word *sp) {
  return transmit(f, stdout, *sp),
         fputc('\n', stdout),
         ip++,
         ip->ap(f, ip, hp, sp); }

static status jump(state f, verb ip, word *hp, word *sp) {
  return ip = (mo) ip[1].x, ip->ap(f, ip, hp, sp); }
static status Kj(state f, verb ip, word *hp, word *sp) {
  Have1();
  *--sp = ip[1].x;
  ip = (mo) ip[2].x;
  return ip->ap(f, ip, hp, sp); }

static status branch(state f, verb ip, word *hp, word *sp) {
  ip = nilp(*sp++) ? ip + 2 : ip[1].m;
  return ip->ap(f, ip, hp, sp); }
static status barnch(state f, verb ip, word *hp, word *sp) {
  ip = nilp(*sp++) ? ip[1].m : ip + 2;
  return ip->ap(f, ip, hp, sp); }

static status ret0(state f, verb ip, word *hp, word *sp) {
  ob r = *sp++;
  ip = (verb) *sp;
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

static status retn(state f, verb ip, word *hp, word *sp) {
  ob r = *sp++;
  sp += getnum(ip[1].x);
  ip = (mo) *sp;
  *sp = r;
  return ip->ap(f, ip, hp, sp); }

static status apply(state f, verb ip, word *hp, word *sp) {
  if (nump(sp[1])) ip++;
  else {
    mo j = (mo) sp[1];
    sp[1] = (ob) (ip + 1);
    ip = j; }
  return ip->ap(f, ip, hp, sp); }

static status add2(state f, verb ip, word *hp, word *sp) {
  return sp[1] += sp[0] - 1,
         ip[1].ap(f, ip+1, hp, sp+1); }

static status curry(state f, verb ip, word *hp, word *sp) {
  const size S = 3 + Width(struct tag);
  Have(2 * S);
  intptr_t n = getnum(ip[1].x);
  verb c0 = (mo) hp, c1;
  hp += S;
  c0[0].ap = Kj;
  c0[1].x = *sp++;
  c0[2].x = ip[2].x;
  if (n > 2)
    c1 = c0 + S,
    hp += S,
    c1[0].ap = curry,
    c1[1].x = putnum(n - 1),
    c1[2].x = (ob) c0,
    c0 = c1;
  ip = (mo) *sp;
  *sp = (ob) c0;
  return ip->ap(f, ip, hp, sp); }

static void test_print_number(state f, int n) {
  assert(f->ip = thd(f, K, putnum(n), show1, yield, End));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(9)); }

static void test_currying(state f) {
  verb k;
  assert(k = thd(f, add2, add2, yield, End));
  assert(k = thd(f, curry, putnum(3), k, End));
  assert(f->ip = thd(f, K, k, K, putnum(1), apply, K, putnum(2), apply, K, putnum(3), apply, End));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(6)); }

static status diag(state f, verb ip, word *hp, word *sp) {
  Have1();
  sp[-1] = sp[0];
  ip++, sp--;
  return ip->ap(f, ip, hp, sp); }

static status expect(state f, verb ip, word *hp, word *sp) {
  word want = ip++->x, got = *sp++;
  assert(eql(f, want, got));
  return ip->ap(f, ip, hp, sp); }


struct cctx {
  word s1, s2, eb, ib, sb;
  struct cctx *par; };

static struct cctx *scope(state f, struct cctx **par) {
  struct cctx *sc = (void*) mo_n(f, Width(struct cctx));
  if (sc)
    sc->s1 = sc->s2 = sc->eb = sc->ib = sc->sb = nil,
    sc->par = par ? *par : (void*) nil;
  return sc; }

static verb yield_thread(state f, struct cctx **c, verb k) { return k; }
static verb pull_thread(state f, struct cctx **c, verb k) { return
  ((mo (*)(state, struct cctx**, mo)) (*f->sp++))(f, c, k); }
static verb e1(state f, struct cctx **c, verb k) { return
  (--k)->x = *f->sp++, pull_thread(f, c, k); }
static verb e2(state f, struct cctx **c, verb k) { return
  k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static verb var(state f, struct cctx **c, verb k) {
  k[-2].ap = K, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static size ana(state, struct cctx**, size, word);
static verb cata(O f, struct cctx **c, size m) { mo k; return
  !(k = mo_n(f, m)) ? k : pull_thread(f, c, k + m); }

static void test_ana_cata(state f) {
  intptr_t m, x;
  union mo
    j[] = { {add2}, {add2}, {show1}, {yield} },
    k[] = { {curry}, {.x = putnum(3)}, {.m = j} };
  assert(x = list(f, k, putnum(3), End)),
  assert(x = list(f, x, putnum(2), putnum(1), End)),
  assert(f->ip = compile_expression(f, x)),
  assert(li_go(f) == Ok),
  assert(pop1(f) == putnum(6)); }

static void test_quote(state f) {
  union mo y[] = { {yield} };
  ob x;
  assert(x = (ob) strof(f, Quote));
  assert(x = list(f, x, x, End));
  assert(x = list(f, y, x, End));
  assert(f->ip = compile_expression(f, x));
  assert(li_go(f) == Ok);
  assert(kstrq((str) pop1(f), Quote)); }


static void test_cond(state f) {
  union mo y[] = { {yield} };
  assert(Ok == receive2(f, "(? 0 1 2 3 4)"));
  word x;
  assert(x = list(f, y, pop1(f), End));
  assert(f->ip = compile_expression(f, x));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(3)); }

static void test_receive2(state f) {
  assert(receive2(f, "99") == Ok);
  assert(pop1(f) == putnum(99));
  assert(receive2(f, "((9 a) 2)") == Ok);
  word x = pop1(f);
  assert(twop(x) && twop(B(x)) && nilp(B(B(x))));
  assert(twop(A(x)) && A(A(x)) == putnum(9));
  assert(twop(B(A(x))) && nilp(B(B(A(x)))) && strp(A(B(A(x)))));
  assert(A(B(x)) == putnum(2)); }

static void test_lambda(state f) {
}

status self_test(O f) {
  test_big_list(f);
  test_print_number(f, 9);
  test_currying(f);
  test_ana_cata(f);
  test_quote(f);
  test_cond(f);
  test_receive2(f);
  test_lambda(f);
  return Ok; }


static verb action_of(state f, struct cctx **c, word x) {
  return (x = pushs(f, x, yield_thread, End)) &&
         (x = ana(f, c, 0, pop1(f))) ?
           cata(f, c, x) :
           0; }

static verb compile_expression(state f, word x) {
  verb k = 0;
  struct cctx *c = push1(f, x) ? scope(f, NULL) : NULL;
  if (c) avec(f, c, k = action_of(f, &c, pop1(f)));
  return k; }

static bool kstrq(str s0, const char *s1) { return
  strlen(s1) == s0->len && strncmp(s0->text, s1, s0->len) == 0; }

static size value(state f, struct cctx**c, size m, word x) {
  return pushs(f, e2, K, x, End) ? m + 2 : 0; }
static size ana_str(state f, struct cctx **c, size m, word x) {
  return pushs(f, var, x, End) ? m + 2 : 0; }

static size ana_two(state, struct cctx**, size, word);

static size ana(state f, struct cctx **c, size m, word x) {
  return twop(x) ? ana_two(f, c, m, x) :
         strp(x) ? ana_str(f, c, m, x) :
                   value(f, c, m, x); }

static verb
  ana_cond_push_continuation(state, struct cctx **, verb),
  ana_cond_push_alternative(state, struct cctx**, verb),
  ana_cond_pop_alternative(state, struct cctx**, verb),
  ana_cond_pop_continuation(state, struct cctx**, verb);
static size
  ana_cond_loop(state, struct cctx**, size, word);

static verb ana_cond_pop_alternative(state f, struct cctx**c, verb k) {
  (--k)->x = A((*c)->s1);
  (--k)->ap = barnch;
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
    m = ana(f, c, m + 2, A(B(x)));
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
  x = snoced(f, x);
  if (!x || !push1(f, x)) return 0;
  struct cctx *d = scope(f, c);
  if (!d) return 0;
  d->eb = d->sb = pop1(f);
  avec(f, d,
    x = (word) action_of(f, &d, pop1(f)),
    x = x && twop(d->ib) ? (word) pair(f, x, d->ib) : x);
  return x ? ana(f, c, m, x) : x; }

static size ana_two(state f, struct cctx **c, size m, word x) {
  if (strp(A(x))) {
    str s = (str) A(x);
    if (kstrq(s, Quote)) return value(f, c, m, twop(B(x)) ? A(B(x)) : B(x));
    if (kstrq(s, Cond)) return ana_cond(f, c, m, B(x));
    if (kstrq(s, Lambda)) return ana_lambda(f, c, m, B(x)); }
  for (MM(f, &x), m = ana(f, c, m, A(x)), x = B(x); m && twop(x); x = B(x))
    m = ana(f, c, m + 1, A(x)),
    m = pushs(f, e1, apply, End) ? m : 0;
  return UM(f), m; }

static verb ana_cond_push_continuation(state f, struct cctx **c, verb k) {
  two w = pair(f, (ob) k, (*c)->s2);
  return !w ? (verb) w : pull_thread(f, c, (verb) A((*c)->s2 = (ob) w)); }

static verb ana_cond_push_alternative(state f, struct cctx**c, verb k) {
  two w = pair(f, (ob) k, (*c)->s1);
  if (!w) return (verb) w;
  k = (verb) A((*c)->s1 = (ob) w);
  verb kk = (verb) A((*c)->s2);
  if (kk->ap == ret0) (--k)->ap = kk->ap;
  else (--k)->m = kk, (--k)->ap = jump;
  return pull_thread(f, c, k); }

static verb ana_cond_pop_continuation(state f, struct cctx **c, verb k) {
  return (*c)->s2 = B((*c)->s2), pull_thread(f, c, k); }
