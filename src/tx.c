#include "la.h"
#include "vm.h"

// s-expression emitter
// FIXME handle stdio errors

// FIXME this is really weird
// print a function name
static int emhomn(la v, FILE *o, ob x) {
  if (symp(x)) return fputc('\\', o), 1 + tx(v, o, x);
  if (!twop(x)) return fputc('\\', o), 1;
  int r = 0;
  // FIXME this is weird
  if (symp(A(x)) || twop(A(x))) r += emhomn(v, o, A(x));
  if (symp(B(x)) || twop(B(x))) r += emhomn(v, o, B(x));
  return r; }

// s-expression writer
int tx(la v, FILE *o, ob x) {
  if (nump(x)) return fprintf(o, "%ld", getnum(x));
  if (primp(x)) return fprintf(o, "\\%s", ((struct prim*)x)->nom);
  if (G(x) == disp) return ((mtbl) GF(x))->emit(v, o, x);
  return emhomn(v, o, hnom(v, x)); }

Vm(show_u) {
  size_t i = 0, l = fp->argc;
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
