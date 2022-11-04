#include "la.h"
#include <time.h>

// wrap the call so the vm function doesn't release
// a stack pointer (which would break TCO)
static NoInline void gettime_call(la v) {
  struct timespec ts;
  // FIXME check for error
  clock_gettime(CLOCK_REALTIME, &ts);
  two w = pair(v, putnum(ts.tv_sec), putnum(ts.tv_nsec));
  v->xp = w ? (ob) w : nil; }

Vm(gettime) { return
  CallOut(gettime_call(v)),
  ApC(ret, xp); }
