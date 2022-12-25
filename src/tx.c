#include "la.h"

static void tx_mo_n(la, la_io, ob);

// s-expression writer
u0 transmit(la v, la_io o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == disp) ((mtbl) GF(x))->emit(v, o, x);
  else tx_mo_n(v, o, hnom(v, (mo) x)); }

u0 la_putsn(const char *s, size_t n, la_io o) {
  while (n--) putc(*s++, o); }

// FIXME this is really weird
// print a function name
static NoInline void tx_mo_n(la v, la_io o, ob x) {
  if (symp(x)) putc('\\', o), transmit(v, o, x);
  else if (!twop(x)) putc('\\', o);
  else {
    if (symp(A(x)) || twop(A(x))) tx_mo_n(v, o, A(x)); 
    if (symp(B(x)) || twop(B(x))) tx_mo_n(v, o, B(x)); } }

Vm(tx_f) {
  size_t i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      transmit(v, stdout, fp->argv[i++]),
      putc(' ', stdout);
    xp = fp->argv[i];
    transmit(v, stdout, xp); }
  putc('\n', stdout);
  return ApC(ret, xp); }

Vm(txc_f) {
  ArityCheck(1);
  return ApC(ret,
    putnum(putc(getnum(fp->argv[0]), stdout))); }
