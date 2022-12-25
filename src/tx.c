#include "la.h"

u0 la_putsn(const char *s, size_t n, la_io o) {
  while (n--) putc(*s++, o); }

static u0 tx_nom(la, la_io, ob);
u0 transmit(la v, la_io o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == data) ((typ) GF(x))->emit(v, o, x);
  else tx_nom(v, o, hnom(v, (mo) x)); }

// print a function name // this is weird
static NoInline u0 tx_nom(la v, la_io o, ob x) {
  if (symp(x)) putc('\\', o), transmit(v, o, x);
  else if (!twop(x)) putc('\\', o);
  else {
    if (symp(A(x)) || twop(A(x))) tx_nom(v, o, A(x)); 
    if (symp(B(x)) || twop(B(x))) tx_nom(v, o, B(x)); } }

Vm(txc_f) { return !fp->argc ?
  ApC(xary, putnum(1)) :
  ApC(ret, putnum(putc(getnum(fp->argv[0]), stdout))); }

Vm(tx_f) {
  U i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      transmit(v, stdout, fp->argv[i++]),
      putc(' ', stdout);
    xp = fp->argv[i];
    transmit(v, stdout, xp); }
  return putc('\n', stdout), ApC(ret, xp); }
