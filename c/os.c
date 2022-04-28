#include "lips.h"
#include "terp.h"
#include <time.h>
Vm(exit_u) { exit(N(Argc) ? N(*Argv) : EXIT_SUCCESS); }
Vm(sys_u) { Ary(1);
            Tc(*Argv, Str);
            str s = S(*Argv);
            xp = _N(system(s->text));
            Jump(ret); }

Vm(clock_u) {
  struct timespec ts;
  if (clock_gettime(CLOCK_REALTIME, &ts) == 0)
    xp = _N(ts.tv_sec * 1000 + ts.tv_nsec / 1000000);
  Jump(ret); }

Vm(sleep_u) {
  Ary(1);
  Tc(*Argv, Num);
  struct timespec ts;
  ldiv_t ld = ldiv(N(*Argv), 1000);
  ts.tv_sec = ld.quot;
  ts.tv_nsec = ld.rem * 1000000;
  nanosleep(&ts, NULL);
  Jump(ret); }
