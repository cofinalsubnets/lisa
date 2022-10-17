#include "lisa.h"

// s-expression writer
void tx(la v, FILE *o, ob x) {
  switch (TypeOf(x)) {
    case Hom: emhom(v, o, x); return;
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
