#include "lips.h"
#include "terp.h"
VM(exit_u) { 
  u64 argc = N(Argc);
  if (!argc) exit(EXIT_SUCCESS);
  TC(*Argv, Num);
  exit(N(*Argv)); }
