#include "i.h"

// code emitter

Vm(txc_f) { return !fp->argc ?
  Yield(ArityError, putnum(1)) :
  ApC(ret, putnum(putc(getnum(fp->argv[0]), stdout))); }

Vm(tx_f) {
  U i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      transmit(v, stdout, fp->argv[i++]),
      putc(' ', stdout);
    xp = fp->argv[i];
    transmit(v, stdout, xp); }
  return putc('\n', stdout),
         ApC(ret, xp); }

static void tx_nom(la, FILE*, ob);
void transmit(la v, FILE* o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == act) ((typ) GF(x))->emit(v, o, x);
  else tx_nom(v, o, hnom(v, (mo) x)); }

// print a function name // this is weird
static NoInline void tx_nom(la v, FILE* o, ob x) {
  if (symp(x)) putc('\\', o), transmit(v, o, x);
  else if (!twop(x)) putc('\\', o);
  else { if (symp(A(x)) || twop(A(x))) tx_nom(v, o, A(x));
         if (symp(B(x)) || twop(B(x))) tx_nom(v, o, B(x)); } }
