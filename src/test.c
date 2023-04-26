#include "i.h"

static void test_big_list(O f) {
  size_t n = 1 << 20;
  for (size_t i = 0; i < n; i++)
    assert(push1(f, nil));
  for (ob l = nil; n--;)
    assert(l = (ob) pair(f, pop1(f), l)),
    assert(l = (ob) pair(f, l, l)); }

#define VmData ip, hp, sp
#define Pack(f) (f->ip = ip, f->hp = hp, f->sp = sp)
#define Unpack(f) (ip = f->ip, hp = f->hp, sp = f->sp)
#define Have(f, n) if (avail(f) < n) return (f->ip = n, gc(f, VmData))
    /*
static enum status yield(O f) {
  return Ok; }
  */

enum status self_test(O f) {
  test_big_list(f);
  return Ok; }
