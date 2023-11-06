#include "i.h"


static enum status NoInline receive2(state f, const char *_i) {
  size_t len = strlen(_i);
  char *i = malloc(len + 1);
  if (!i) return Oom;
  memcpy(i, _i, len);
  i[len] = 0;
  FILE *in = fmemopen(i, len, "r");
  if (!in) {
    free(i);
    return Oom; }
  enum status s = read_source(f, in);
  fclose(in);
  free(i);
  return s; }

static void
  test_biglist(state),
  test_number(state),
  test_quote(state),
  test_cond(state),
  test_receive2(state),
  test_lambda(state),
  test_lambda2(state),
  test_add(state),
  test_let_add(state),
  test_let_fib(state),
  test_closure(state);

static void test_biglist(state f) {
  long n = 1 << 20;
  for (long i = 0; i < n; i++) assert(push1(f, nil));
  for (word l = nil; n--;)
    assert((l = (word) cons(f, pop1(f), l))),
    assert((l = (word) cons(f, l, l))); }

static void test_quote(state f) {
  word x;
  assert((x = (word) cons(f, nil, nil)));
  A(x) = x;
  assert(Ok == eval(f, x));
  x = pop1(f);
  assert(x == A(x)); }

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

static void test_add(state f) {
  char prog[] = "((\\ f (f 9)) (+ 8))";
  assert(Ok == receive2(f, prog));
  assert(Ok == eval(f, pop1(f)));
  assert(pop1(f) == putnum(17)); }

static void test_closure(state f) {
  char prog[] = "((\\ f ((\\ a b (f a b)) 2 3)) (\\ a b a))";
  assert(Ok == receive2(f, prog));
  assert(Ok == eval(f, pop1(f)));
  assert(pop1(f) == putnum(2)); }

static void test_number(state f) {
  assert(Ok == eval(f, putnum(9)));
  assert(pop1(f) == putnum(9)); }

static void test_let_add(state f) {
  char prog[] = "(: a 9 b 7 c -3 (+ a (+ b c)))";
  assert(Ok == receive2(f, prog));
  assert(Ok == eval(f, pop1(f)));
  assert(pop1(f) == putnum(13)); }

static void test_let_fib(state f) {
  char prog[] =
    "(: fib (\\ n"
    "        (? (< n 3) 1"
    "         (+ ((. fib) (+ n -1))"
    "            ((. fib) (+ n -2)))))"
    " (fib 12))";
  assert(Ok == receive2(f, prog));
  assert(Ok == eval(f, pop1(f)));
  word x = pop1(f);
  transmit(f, stdout, x); puts("");
  assert(x == putnum(144)); }

#define TEST(n) (printf("%s:%d "#n"\n", __FILE__, __LINE__), (test_##n)(f))
enum status self_test(state f) {
  printf("# dim=%ld f@0x%lx[len=%ld]\n", sizeof(word), (word) f, f->len);
  TEST(lambda2);
  TEST(lambda);
  TEST(cond);
  TEST(receive2);
  TEST(number);
  TEST(closure);
  TEST(quote);
  TEST(biglist);
  TEST(add);
  TEST(let_add);
  TEST(let_fib);
  return Ok; }