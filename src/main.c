#include "i.h"
#include <unistd.h>

int main(int ac, char **av) {
  struct V v;
  enum status s = li_ini(&v);
  if (s != Ok) return EXIT_FAILURE;
  li_fin(&v);
  return EXIT_SUCCESS; }
