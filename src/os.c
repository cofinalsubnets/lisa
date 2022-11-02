#include "la.h"
#include <time.h>

Vm(gettime) {
  Have(Width(two));
  struct timespec ts;
  // FIXME check for error
  clock_gettime(CLOCK_REALTIME, &ts);
  two w = ini_two(hp, putnum(ts.tv_sec), putnum(ts.tv_nsec));
  hp += Width(two);
  return ApC(ret, (ob) w); }
