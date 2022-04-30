#include "lips.h"
#include "ll.h"
#include <time.h>
Vm(exit_u) { exit(getnum(Argc) ? getnum(*Argv) : EXIT_SUCCESS); }
Vm(sys_u) { Arity(1);
            TypeCheck(*Argv, Str);
            str s = getstr(*Argv);
            return ApC(ret, putnum(system(s->text))); }

Vm(clock_u) {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) == 0)
    xp = putnum(ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
  return ApC(ret, xp); }

Vm(sleep_u) {
  Arity(1);
  TypeCheck(*Argv, Num);
  struct timespec ts;
  ldiv_t ld = ldiv(getnum(*Argv), 1000);
  ts.tv_sec = ld.quot;
  ts.tv_nsec = ld.rem * 1000000;
  nanosleep(&ts, NULL);
  return ApC(ret, xp); }
