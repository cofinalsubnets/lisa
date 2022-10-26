#include "lisa.h"
#include "vm.h"
#include <string.h>
// isolate the more complicated logic from the simple
// pointer comparisons so eql() doesn't touch the stack
// unless it has to
static NoInline bool eql_two(two a, two b) {
  return eql(a->a, b->a) ? eql(a->b, b->b) : false; }

static NoInline bool eql_str(str a, str b) {
  return a->len == b->len && 0 == scmp(a->text, b->text); }

static NoInline bool eql_(ob a, ob b) {
  if (G(a) != disp || G(b) != disp || GF(a) != GF(b)) return false;
  if (GF(a) == (vm*) mtbl_two) return eql_two((two) a, (two) b);
  if (GF(a) == (vm*) mtbl_str) return eql_str((str) a, (str) b);
  return false; }

bool eql(ob a, ob b) {
  return a == b ? true : nump(a|b) ? false : eql_(a, b); }
