#include "i.h"

static NoInline void tx_mo_nom(la v, FILE* o, ob x) {
  if (symp(x)) putc('\\', o), transmit(v, o, x);
  else if (!twop(x)) putc('\\', o);
  else {
    if (symp(A(x)) || twop(A(x))) tx_mo_nom(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) tx_mo_nom(v, o, B(x)); } }

void transmit(la v, FILE* o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == act) ((typ) GF(x))->emit(v, o, x);
  else tx_mo_nom(v, o, hnom(v, (mo) x)); }
