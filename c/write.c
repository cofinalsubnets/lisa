#include "la.h"

static void emhomn(em v, ob x, FILE *o) {
  if (symp(x)) fputc(Backslash, o), emit(v, x, o);
  else if (!twop(x)) fputc(Backslash, o);
  else { if (symp(A(x)) || twop(A(x))) emhomn(v, A(x), o);
         if (symp(B(x)) || twop(B(x))) emhomn(v, B(x), o); } }

static void emhom(em v, ob x, FILE *o) {
  emhomn(v, homnom(v, x), o); }

static void emnum(em v, ob x, FILE *o) {
  fprintf(o, "%ld", getnum(x)); }

static void emstr(em v, ob x, FILE *o) {
  str s = getstr(x);
  fputc(DoubleQuote, o);
  for (char *t = s->text; *t; fputc(*t++, o))
    if (*t == DoubleQuote) fputc(Backslash, o);
  fputc(DoubleQuote, o); }

static void emtbl(em v, ob x, FILE *o) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

static void emsym(em v, ob x, FILE *o) {
  sym y = getsym(x);
  (nilp(y->nom) ?
    fprintf(o, "#sym@%lx", (long) y) :
    fputs(getstr(y->nom)->text, o)); }

static void emtwo(em v, ob x, FILE *o) {
  // quotation?
  if (A(x) == v->lex[Quote] &&
      twop(B(x)) &&
      nilp(BB(x)))
    return fputc(SingleQuote, o), emit(v, A(B(x)), o);

  for (fputc(LeftParen, o);; x = B(x)) {
    emit(v, A(x), o);
    if (!twop(B(x))) break;
    fputc(Space, o); }

  fputc(RightParen, o); }


Inline void emit(em v, ob x, FILE *o) {
  static void (*ems[])(em, ob, FILE*) = {
    [Hom] = emhom, [Num] = emnum, [Sym] = emsym,
    [Two] = emtwo, [Str] = emstr, [Tbl] = emtbl, };
  return ems[Q(x)](v, x, o); }

// print to console
Ll(show_u) {
  uintptr_t i, l = getnum(fp->argc);
  if (l) {
    for (i = 0; i < l - 1; i++)
      emit(v, fp->argv[i], stdout),
      fputc(Space, stdout);
    emit(v, xp = fp->argv[i], stdout); }
  return fputc(Newline, stdout),
         ApC(ret, xp); }

Ll(putc_u) {
  Arity(1);
  return fputc(getnum(*fp->argv), stdout),
         ApC(ret, xp); }
