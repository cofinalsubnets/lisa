#include "i.h"

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
  Have1(); return
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

static status ret0(state f, verb ip, word *hp, word *sp) {
  ob r = *sp++;
  ip = (verb) *sp, *sp = r;
  return ip->ap(f, ip, hp, sp); }

static status retn(state f, verb ip, word *hp, word *sp) {
  ob r = *sp++;
  sp += getnum(ip[1].x);
  ip = (mo) *sp, *sp = r;
  return ip->ap(f, ip, hp, sp); }

static status apply(state f, verb ip, word *hp, word *sp) { mo j; return
  j = (mo) sp[1], sp[1] = (ob) (ip + 1), j->ap(f, j, hp, sp); }

static status add2(state f, verb ip, word *hp, word *sp) { return
  sp[1] += sp[0] - 1, ip++, sp++, ip->ap(f, ip, hp, sp); }

static status curry(state f, verb ip, word *hp, word *sp) {
  const size S = 3 + Width(struct tag);
  Have(2 * S);
  Z n = getnum(ip[1].x);
  mo c0 = (mo) hp, c1;
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

static void test_print_number(state f, Z n) {
  assert(f->ip = thd(f, K, putnum(n), show1, yield, End));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(9)); }

static void test_currying(state f) {
  mo k;
  assert(k = thd(f, add2, add2, yield, End));
  assert(k = thd(f, curry, putnum(3), k, End));
  assert(f->ip = thd(f, K, k, K, putnum(1), apply, K, putnum(2), apply, K, putnum(3), apply, End));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(6)); }

struct cctx {
  ob dict;
  struct cctx *par; };

static verb yield_thread(state f, struct cctx **c, verb k) { return k; }
static verb pull_thread(state f, struct cctx **c, verb k) { return
  ((mo (*)(state, struct cctx**, mo)) (*f->sp++))(f, c, k); }
static verb e1(state f, struct cctx **c, verb k) { return
  (--k)->x = *f->sp++, pull_thread(f, c, k); }
static verb e2(O f, struct cctx **c, verb k) { return
  k[-2].x = *f->sp++, k[-1].x = *f->sp++, pull_thread(f, c, k - 2); }

static size ana(state f, struct cctx **c, size m, word x) {
  if (!twop(x)) return pushs(f, e2, K, x, End) ? m + 2 : 0;
  for (MM(f, &x), m = ana(f, c, m, A(x)), x = B(x); m && twop(x); x = B(x))
    m = ana(f, c, m + 1, A(x)),
    m = pushs(f, e1, apply, End) ? m : 0;
  return UM(f), m; }

static mo cata(O f, struct cctx **c, intptr_t m) { mo k; return
  !(k = mo_n(f, m)) ? k : pull_thread(f, c, k + m); }

static void test_ana_cata(state f) {
  intptr_t m, x;
  union mo
    j[] = { {add2}, {add2}, {show1}, {yield} },
    k[] = { {curry}, {.x = putnum(3)}, {.m = j} };
  assert(pushs(f, yield_thread, End)),
  assert(x = list(f, k, putnum(3), putnum(2), putnum(1), End)),
  assert(m = ana(f, NULL, 0, x)),
  assert(f->ip = cata(f, NULL, m)),
  assert(li_go(f) == Ok),
  assert(pop1(f) == putnum(6)); }

static bool kstrq(str s0, const char *s1) { return
  strlen(s1) == s0->len && strncmp(s0->text, s1, s0->len) == 0; }

enum status self_test(O f) { return
  test_big_list(f),
  test_print_number(f, 9),
  test_currying(f),
  test_ana_cata(f),
  Ok; }
