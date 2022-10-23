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
  switch (TypeOf(x)) {
    case Num: fprintf(o, "%ld", getnum(x)); return;
    case Two: em_two(v, o, x); return;
    case Sym: em_sym(v, o, x); return;
    case Tbl: em_tbl(v, o, x); return;
    case Str: em_str(v, o, (ob) getstr(x)); return; }
  if (primp(x)) fprintf(o, "\\%s", ((struct prim*)x)->nom);
  else if (G(x) == disp) ((mtbl) GF(x))->emit(v, o, x);
  else emhomn(v, o, hnom(v, x)); }

Vm(show_u) {
  size_t i = 0, l = getnum(Argc);
  if (l) {
    while (i < l - 1)
      tx(v, stdout, Argv[i++]),
      fputc(' ', stdout);
    tx(v, stdout, xp = Argv[i]); }
  fputc('\n', stdout);
  return ApC(ret, xp); }

Vm(putc_u) {
  ArityCheck(1);
  fputc(getnum(Argv[0]), stdout);
  return ApC(ret, xp); }
