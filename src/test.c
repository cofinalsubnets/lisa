#include "i.h"
static status add2(state, verb, word*, word*),
              show1(state, verb, word*, word*);

static void
  test_big_list(state),
  test_print_number(state, int),
  test_currying(state),
  test_ana_cata(state),
  test_quote(state),
  test_cond(state),
  test_receive2(state),
  test_lambda(state),
  test_lambda2(state),
  test_closure(state);

status self_test(O f) {
  //test_big_list(f);
  printf("%d\n", __LINE__);
  test_print_number(f, 9);
  test_currying(f);
  test_ana_cata(f);
  test_quote(f);
  test_cond(f);
  test_receive2(f);
  test_lambda(f);
  test_lambda2(f);
  test_closure(f);
  return Ok; }

static void test_big_list(state f) {
  size n = 1 << 20;
  for (size i = 0; i < n; i++)
    assert(push1(f, nil));
  for (ob l = nil; n--;)
    assert((l = (ob) pair(f, pop1(f), l))),
    assert((l = (ob) pair(f, l, l))); }

static void test_ana_cata(state f) {
  intptr_t x;
  union mo
    j[] = { {add2}, {add2}, {retn}, {.x=nil} },
    k[] = { {curry}, {.x = putnum(3)}, {.m = j} };
  assert((x = list(f, k, putnum(3), End))),
  assert((x = list(f, x, putnum(2), putnum(1), End))),
  assert((f->ip = compile_expression(f, x))),
  assert(li_go(f) == Ok),
  assert(pop1(f) == putnum(6)); }

static void test_quote(state f) {
  ob x;
  assert((x = (ob) strof(f, Quote)));
  assert((x = list(f, x, x, End)));
  assert((f->ip = compile_expression(f, x)));
  assert(li_go(f) == Ok);
  assert(kstrq((str) pop1(f), Quote)); }

static void test_cond(state f) {
  union mo y[] = { {yield} };
  assert(Ok == receive2(f, "(? 0 1 2 3 4)"));
  word x;
  assert((x = list(f, y, pop1(f), End)));
  assert((f->ip = compile_expression(f, x)));
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
  char prog1[] = "(\\ a b a)",
       prog2[] = "((\\ a b a) 2 3)";

  assert(Ok == receive2(f, prog1));
  assert((f->ip = compile_expression(f, pop1(f))));
  assert(li_go(f) == Ok);
  assert(homp(pop1(f)));

  assert(Ok == receive2(f, prog2));
  assert((f->ip = compile_expression(f, pop1(f))));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(2)); }

static void test_lambda2(state f) {
  char prog[] = "((\\ f g (f g 1 2 3)) (\\ g a b c (g c b)) (\\ a b a))";
  assert(Ok == receive2(f, prog));
  assert((f->ip = compile_expression(f, pop1(f))));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(3)); }

static void test_closure(state f) {
  char prog[] = "((\\ f ((\\ a b (f a b)) 2 3)) (\\ a b a))";
  assert(Ok == receive2(f, prog));
  assert((f->ip = compile_expression(f, pop1(f))));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(2)); }

static void test_print_number(state f, int n) {
  assert((f->ip = thd(f, K, putnum(n), yield, End)));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(9)); }

static void test_currying(state f) {
  verb k;
  assert((k = thd(f, add2, add2, retn, nil, End)));
  assert((k = thd(f, curry, putnum(3), k, End)));
  assert((f->ip = thd(f, K, k, K, putnum(1), apply, K, putnum(2), apply, K, putnum(3), apply, yield, End)));
  assert(li_go(f) == Ok);
  assert(pop1(f) == putnum(6)); }

static status add2(state f, verb ip, word *hp, word *sp) {
  return sp[1] += sp[0] - 1,
         ip[1].ap(f, ip+1, hp, sp+1); }

static status show1(state f, verb ip, word *hp, word *sp) {
  return transmit(f, stdout, *sp),
         fputc('\n', stdout),
         ip++,
         ip->ap(f, ip, hp, sp); }
