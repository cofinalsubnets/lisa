#include "la.h"
#include <string.h>

// s-expression emitter

// s-expression writer
long la_tx(la v, FILE *o, ob x) {
  if (nump(x)) return fprintf(o, "%ld", getnum(x));
  if (G(x) == disp) return ((mtbl) GF(x))->emit(v, o, x);
  return txmo(v, o, (mo) x); }

Vm(tx_f) {
  size_t i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      la_tx(v, stdout, fp->argv[i++]),
      fputc(' ', stdout);
    xp = fp->argv[i];
    la_tx(v, stdout, xp); }
  fputc('\n', stdout);
  return ApC(ret, xp); }

Vm(txc_f) {
  ArityCheck(1);
  return ApC(ret,
    putnum(fputc(getnum(fp->argv[0]), stdout))); }

long fputstr(FILE *o, str s) {
  long i = 0, r = s->len;
  while (i < r) if (fputc(s->text[i++], o) == EOF) return -1;
  return r; }
