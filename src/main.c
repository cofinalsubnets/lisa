#include "i.h"
#include <stdarg.h>

static enum status go(state f) {
  self_test(f);
  // echo loop
  intptr_t s, height = f->pool + f->len - f->sp;
  while ((s = rx_file(f, stdin)) != Eof) {
    if (s == Ok && (s = eval(f, pop1(f))) == Ok)
      transmit(f, stdout, pop1(f)),
      fputc('\n', stdout);
    else // there was an error
      fprintf(stderr, "# status %ld\n", s),
      f->sp = f->pool + f->len - height; }
  return Ok; }

int main(int ac, char **av) {
  state f = &((struct G){});
  enum status s = l_ini(f);
  if (s == Ok) s = go(f), l_fin(f);
  return s; }

