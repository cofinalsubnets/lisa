#include "i.h"

static void test_big_list(O f) {
  size_t n = 1 << 20;
  for (size_t i = 0; i < n; i++)
    assert(push1(f, nil));
  for (ob l = nil; n--;)
    assert(l = (ob) pair(f, pop1(f), l)),
    assert(l = (ob) pair(f, l, l)); }

static enum status yield(O f, mo ip, ob *hp, ob *sp) {
  return Pack(), Ok; }

static enum status K(O f, mo ip, ob *hp, ob *sp) {
  Have1(); return
    *--sp = ip[1].x,
    ip += 2,
    ip->ap(f, ip, hp, sp); }

static enum status show1(O f, mo ip, ob *hp, ob *sp) {
  return transmit(f, stdout, *sp),
         fputc('\n', stdout),
         ip++,
         ip->ap(f, ip, hp, sp); }

static enum status jump(O f, mo ip, ob *hp, ob *sp) {
  return ip = (mo) ip[1].x, ip->ap(f, ip, hp, sp); }
static enum status Kj(O f, mo ip, ob *hp, ob *sp) {
  Have1();
  *--sp = ip[1].x;
  ip = (mo) ip[2].x;
  return ip->ap(f, ip, hp, sp); }

static enum status ret0(state f, verb ip, word *hp, word *sp) {
  ob r = *sp++;
  ip = (verb) *sp, *sp = r;
  return ip->ap(f, ip, hp, sp); }

static enum status retn(O f, mo ip, ob *hp, ob *sp) {
  ob r = *sp++;
  sp += getnum(ip[1].x);
  ip = (mo) *sp, *sp = r;
  return ip->ap(f, ip, hp, sp); }

static enum status apply(O f, mo ip, ob *hp, ob *sp) {
  mo j = (mo) sp[1];
  sp[1] = (ob) (ip + 1);
  return j->ap(f, j, hp, sp); }

static enum status add2(O f, mo ip, ob *hp, ob *sp) {
  sp[1] += sp[0] - 1;
  ip++, sp++;
  return ip->ap(f, ip, hp, sp); }

static enum status curry(O f, mo ip, ob *hp, ob *sp) {
  const size_t S = 3 + Width(struct tag);
  Have(2 * S);
  intptr_t n = getnum(ip[1].x);
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

static void test_print_number(O f, intptr_t n) {
  assert(f->ip = thd(f, K, putnum(n), show1, yield, End));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(9)); }

static void test_currying(O f) {
  mo k;
  assert(k = thd(f, add2, add2, yield, End));
  assert(k = thd(f, curry, putnum(3), k, End));
  assert(f->ip = thd(f, K, k, K, putnum(1), apply, K, putnum(2), apply, K, putnum(3), apply, End));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(6)); }

struct cctx {
  ob dict;
  struct cctx *par; };

static mo yield_thread(O f, struct cctx *c, mo k) {
  return k; }
static mo pull_thread(O f, struct cctx *c, mo k) {
  return ((mo (*)(O, struct cctx*, mo)) (*f->sp++))(f, c, k); }

static mo e1(O f, struct cctx *c, mo k) {
  (--k)->x = *f->sp++;
  return pull_thread(f, c, k); }
static mo e2(O f, struct cctx *c, mo k) {
  k[-2].x = *f->sp++;
  k[-1].x = *f->sp++;
  return pull_thread(f, c, k - 2); }

static intptr_t ana(O f, struct cctx *c, intptr_t m, ob x) {
  if (!twop(x)) return pushs(f, e2, K, x, End) ? m + 2 : 0;
  MM(f, &x);
  m = ana(f, c, m, A(x));
  for (x = B(x); m && twop(x); x = B(x))
    m = ana(f, c, m + 1, A(x)),
    m = pushs(f, e1, apply, End) ? m : 0;
  UM(f);
  return m; }

static mo cata(O f, struct cctx *c, intptr_t m) {
  mo k = mo_n(f, m);
  return !k ? k : pull_thread(f, c, k + m); }

static void test_ana_cata(O f) {
  intptr_t m, x;
  union mo
    j[] = { {add2}, {add2}, {show1}, {yield} },
    k[] = { {curry}, {.x = putnum(3)}, {.m = j} };
  assert(pushs(f, yield_thread, End));
  assert(x = list(f, k, putnum(3), putnum(2), putnum(1), End));
  assert(m = ana(f, NULL, 0, x));
  assert(f->ip = cata(f, NULL, m));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(6)); }

enum status self_test(O f) {
  test_big_list(f);
  test_print_number(f, 9);
  test_currying(f);
  test_ana_cata(f);
  return Ok; }
