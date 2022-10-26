#include "lisa.h"
#include "vm.h"

// FIXME this is really weird
// print a function name
static void emhomn(la v, FILE *o, ob x) {
  if (symp(x)) fputc('\\', o), tx(v, o, x);
  else if (!twop(x)) fputc('\\', o);
  else { // FIXME this is weird
    if (symp(A(x)) || twop(A(x))) emhomn(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) emhomn(v, o, B(x)); } }

// s-expression writer
void tx(la v, FILE *o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (primp(x)) fprintf(o, "\\%s", ((struct prim*)x)->nom);
  else if (G(x) == disp) ((mtbl) GF(x))->emit(v, o, x);
  else emhomn(v, o, hnom(v, x)); }

Vm(show_u) {
  size_t i = 0, l = getnum(fp->argc);
  if (l) {
    while (i < l - 1)
      tx(v, stdout, fp->argv[i++]),
      fputc(' ', stdout);
    tx(v, stdout, xp = fp->argv[i]); }
  fputc('\n', stdout);
  return ApC(ret, xp); }

Vm(putc_u) {
  ArityCheck(1);
  fputc(getnum(fp->argv[0]), stdout);
  return ApC(ret, xp); }
