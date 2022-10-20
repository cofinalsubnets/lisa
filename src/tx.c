#include "lisa.h"
#include "vm.h"

static void emhomn(la, FILE*, ob);

// s-expression writer
void tx(la v, FILE *o, ob x) {
  switch (TypeOf(x)) {
    case Hom:
      if (primp(x)) fprintf(o, "\\%s", ((struct prim*)x)->nom);
      else if (G(x) == disp) ((mtbl) GF(x))->show(v, o, x);
      else emhomn(v, o, hnom(v, x));
      return;
    case Num: fprintf(o, "%ld", getnum(x)); return;
    case Two:
      for (fputc('(', o);; x = B(x)) {
        tx(v, o, A(x));
        if (!twop(B(x))) {
          fputc(')', o);
          return; }
        fputc(' ', o); }
    case Sym: {
      sym y = getsym(x);
      strp(y->nom) ?
        fputs(getstr(y->nom)->text, o) :
        fprintf(o, "sym@%lx", (long) y);
      return; }
    case Tbl: {
      tbl t = gettbl(x);
      fprintf(o, "#tbl:%ld/%ld", t->len, t->cap);
      return; }
    case Str:
      fputc('"', o);
      for (char *t = getstr(x)->text; *t; fputc(*t++, o))
        if (*t == '"') fputc('\\', o);
      fputc('"', o);
      return; } }

static void emhomn(la v, FILE *o, ob x) {
  if (symp(x)) fputc('\\', o), tx(v, o, x);
  else if (!twop(x)) fputc('\\', o);
  else { // FIXME this is weird
    if (symp(A(x)) || twop(A(x))) emhomn(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) emhomn(v, o, B(x)); } }

#include "vm.h"
Vm(show_u) {
  size_t i, l = getnum(fp->argc);
  if (l > 0) {
    for (i = 0; i < l - 1; i++)
      tx(v, stdout, fp->argv[i]),
      fputc(' ', stdout);
    xp = fp->argv[i];
    tx(v, stdout, xp); }
  return fputc('\n', stdout),
         ApC(ret, xp); }

Vm(putc_u) {
  ArityCheck(1);
  return fputc(getnum(Argv[0]), stdout),
         ApC(ret, xp); }
