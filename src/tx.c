#include "i.h"

void transmit(core v, FILE* o, word x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (R(x)->ap == data) R(x)[1].typ->emit(v, o, x);
  else fprintf(o, "#%lx", x); }
