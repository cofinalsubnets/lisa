#include "la.h"
Ll(show_u) {
  uintptr_t i, l = getnum(fp->argc);
  if (l) {
    for (i = 0; i < l - 1; i++) {
      tx(v, stdout, fp->argv[i]),
      fputc(Space, stdout); }
    xp = fp->argv[i];
    tx(v, stdout, xp); }
  fputc(Newline, stdout);
  return ApC(ret, xp); }

Ll(putc_u) {
  ArityCheck(1);
  fputc(getZ(fp->argv[0]), stdout);
  return ApC(ret, xp); }

static void emhomn(pt, FILE*, ob), em2(pt, FILE*, ob);

void tx(pt v, FILE *o, ob x) { switch (Q(x)) {
  case Hom: return emhomn(v, o, hnom(v, x));
  case Num: fprintf(o, "%ld", getZ(x)); return;
  case Two: return em2(v, o, x);
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
    fputc(DoubleQuote, o);
    for (char *t = getstr(x)->text; *t; fputc(*t++, o))
      if (*t == DoubleQuote) fputc(Backslash, o);
    fputc(DoubleQuote, o);
    return; } }

static void emhomn(pt v, FILE *o, ob x) {
  if (symp(x)) fputc(Backslash, o), tx(v, o, x);
  else if (!twop(x)) fputc(Backslash, o);
  else { // FIXME ??
    if (symp(A(x)) || twop(A(x)))
      emhomn(v, o, A(x));
    if (symp(B(x)) || twop(B(x)))
      emhomn(v, o, B(x)); } }

static void em2s(pt v, FILE *o, ob x) {
  tx(v, o, A(x));
  if (twop(B(x))) fputc(Space, o), em2s(v, o, B(x));
  else fputc(RightParen, o); }

static void em2(pt v, FILE *o, ob x) {
  bool quotp =
    A(x) == v->lex[Quote] &&
    twop(B(x)) &&
    nilp(BB(x));
  if (quotp) fputc(SingleQuote, o), tx(v, o, A(B(x)));
  else fputc(LeftParen, o), em2s(v, o, x); }
