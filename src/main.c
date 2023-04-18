#include "i.h"
#include <unistd.h>

int main(int ac, char **av) {
  struct V v;
  enum status s = li_ini(&v);
  if (s != Ok) return EXIT_FAILURE;
  for (;;) switch ((s = receive(&v, stdin))) {
    case Ok:
      transmit(&v, stdout, pop1(&v));
      puts("");
      continue;
    default:
      fprintf(stderr, "# status %d\n", s);
      continue;
    case Eof:
      li_fin(&v);
      return EXIT_SUCCESS; } }
