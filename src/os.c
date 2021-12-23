#include "lips.h"
#include "terp.h"
VM(exit_u) { 
  u64 argc = N(ARGC);
  if (!argc) exit(EXIT_SUCCESS);
  TC(*ARGV, Num);
  exit(N(*ARGV)); }
