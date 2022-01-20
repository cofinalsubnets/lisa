#include "lips.h"
#include "terp.h"
#include "str.h"
Vm(exit_u) { exit(N(Argc) ? N(*Argv) : EXIT_SUCCESS); }
Vm(sys_u) {
  Ary(1);
  Tc(*Argv, Str);
  str s = S(*Argv);
  xp = _N(system(s->text));
  Jump(ret); }
