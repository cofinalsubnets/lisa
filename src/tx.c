#include "la.h"

static void tx_mo_n(la, la_io, ob);

// s-expression writer
void la_tx(la v, la_io o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == disp) ((mtbl) GF(x))->emit(v, o, x);
  else if (primp((mo) x))
    fprintf(o, "\\%s", ((struct la_prim*)x)->nom);
  else tx_mo_n(v, o, hnom(v, (mo) x)); }

void la_putsn(const char *s, size_t n, la_io o) {
  while (n--) la_putc(*s++, o); }

// FIXME this is really weird
// print a function name
static NoInline void tx_mo_n(la v, la_io o, ob x) {
  if (symp(x)) la_putc('\\', o), la_tx(v, o, x);
  else if (!twop(x)) la_putc('\\', o);
  else {
    if (symp(A(x)) || twop(A(x))) tx_mo_n(v, o, A(x)); 
    if (symp(B(x)) || twop(B(x))) tx_mo_n(v, o, B(x)); } }

Vm(tx_f) {
  size_t i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      la_tx(v, la_stdout, fp->argv[i++]),
      la_putc(' ', la_stdout);
    xp = fp->argv[i];
    la_tx(v, la_stdout, xp); }
  la_putc('\n', la_stdout);
  return ApC(ret, xp); }

Vm(txc_f) {
  ArityCheck(1);
  return ApC(ret,
    putnum(la_putc(getnum(fp->argv[0]), la_stdout))); }
