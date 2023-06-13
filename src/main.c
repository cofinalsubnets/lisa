#include "i.h"

// echo loop
static status go(state f) {
#ifdef testing
  return self_test(f); }
#else
  // echo loop
  for (intptr_t h = height(f), s; (s = receive(f, stdin)) != Eof;)
    if (s == Ok) transmit(f, stdout, *f->sp++), fputc('\n', stdout);
    else fprintf(stderr, "# status %ld\n", s),
         f->sp = f->pool + f->len - h;
  return Ok; }
#endif

int main(int ac, char **av) {
  state f = &((struct carrier){});
  status s = li_ini(f);
  if (s != Ok) return s;
  printf("# dim=%ld f@0x%lx[len=%ld]\n", sizeof(word), (word) f, f->len);
  if ((s = go(f)) != Ok) fprintf(stderr, "# status=%d\n", s);
  return li_fin(f), s; }
