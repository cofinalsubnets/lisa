#include "i.h"
#include <stdio.h>
static NoInline word listr(state f, va_list xs) {
  word y, x = va_arg(xs, word);
  if (!x) return nil;
  avec(f, x, y = listr(f, xs));
  return y ? (word) pair(f, x, y) : y; }

NoInline word list(state f, ...) {
  word x; va_list xs;
  va_start(xs, f), x = listr(f, xs), va_end(xs);
  return x; }

static status add2(state, verb, word*, word*);
static void
  test_big_list(state),
  test_number(state),
  test_quote(state),
  test_cond(state),
  test_receive2(state),
  test_lambda(state),
  test_lambda2(state),
  test_closure(state);

status self_test(O f) {
  printf("%s:%d\n", __FILE__, __LINE__);
  test_lambda2(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  test_lambda(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  test_cond(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  test_receive2(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  test_number(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  test_closure(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  test_quote(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  test_big_list(f);
  printf("%s:%d\n", __FILE__, __LINE__);
  return Ok; }

static void test_big_list(state f) {
  size n = 1 << 20;
  for (size i = 0; i < n; i++)
    assert(push1(f, nil));
  for (ob l = nil; n--;)
    assert((l = (ob) pair(f, pop1(f), l))),
    assert((l = (ob) pair(f, l, l))); }

static void test_quote(state f) {
  ob x;
  assert((x = (ob) strof(f, Quote)));
  assert((x = list(f, x, x, End)));
  assert(Ok == eval(f, x));
  assert(kstrq((str) pop1(f), Quote)); }

static void test_cond(state f) {
  assert(Ok == receive2(f, "(? 0 1 2 3 4)"));
  assert(Ok == eval(f, pop1(f)));
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
  assert(Ok == eval(f, pop1(f)));
  assert(homp(pop1(f)));
  assert(Ok == receive2(f, prog2));
  assert(Ok == eval(f, pop1(f)));
  assert(pop1(f) == putnum(2)); }

static void test_lambda2(state f) {
  char prog[] = "((\\ f g (f g 1 2 3)) (\\ g a b c (g c b)) (\\ a b a))";
  assert(Ok == receive2(f, prog));
  assert(Ok == eval(f, pop1(f)));
  assert(pop1(f) == putnum(3)); }

static void test_closure(state f) {
  char prog[] = "((\\ f ((\\ a b (f a b)) 2 3)) (\\ a b a))";
  assert(Ok == receive2(f, prog));
  assert(Ok == eval(f, pop1(f)));
  assert(pop1(f) == putnum(2)); }

static void test_number(state f) {
  assert(Ok == eval(f, putnum(9)));
  assert(pop1(f) == putnum(9)); }

static status add2(state f, verb ip, word *hp, word *sp) {
  return sp[1] += sp[0] - 1,
         ip[1].ap(f, ip+1, hp, sp+1); }
