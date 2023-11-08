#include "i.h"
#include <stdarg.h>

status report(state f, status s) {
  switch (s) {
    case Dom:
      fprintf(stderr, "# domain error at [0x%lx]", f->ip->x);
      break;
    case Oom:
      fprintf(stderr, "# out of memory at %ld words", f->len);
    default: }
  return s; }

static status go(state f) {
#ifdef testing
  self_test(f);
#endif
  // repl
  for (status s; (s = read_source(f, stdin)) != Eof;) {
    if (s == Ok && (s = eval(f, pop1(f))) == Ok)
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout);
    else // there was an error
      report(f, s),
      f->sp = f->pool + f->len; }
  return Ok; }

int main(int ac, char **av) {
  state f = &((struct gwen){});
  status s = l_ini(f);
  if (s == Ok) s = go(f), l_fin(f);
  return s; }
