#include "lips.h"
#include "write.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "str.h"
#include "terp.h"

u0 ems(lips v, FILE *o, obj x, char s) {
 emit(v, x, o), fputc(s, o); }

u0 emsep(lips v, obj x, FILE *o, char s) {
 emit(v, x, o), fputc(s, o); }

#include "tbl.h"
#include "hom.h"
#include "vec.h"

static u0 emstr(lips v, str s, FILE *o) {
  fputc('"', o);
  for (char *t = s->text; *t; fputc(*t++, o))
    if (*t == '"') fputc('\\', o);
  fputc('"', o); }

static u0 emtbl(lips v, tbl t, FILE *o) {
  fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }

static u0 emsym(lips v, sym y, FILE *o) {
  y->nom == nil ? fprintf(o, "#sym@%lx", (long) y) :
                  fputs(S(y->nom)->text, o); }

static u0 emtwo_(lips v, two w, FILE *o) {
  twop(w->b) ? (emsep(v, w->a, o, ' '),
                emtwo_(v, gettwo(w->b), o)) :
               emsep(v, w->a, o, ')'); }

static u0 emtwo(lips v, two w, FILE *o) {
  w->a == Qt && twop(w->b) && nilp(B(w->b)) ?
    (fputc('\'', o),
     emit(v, A(w->b), o)) :
    (fputc('(', o),
     emtwo_(v, w, o)); }

static u0 emvec(lips v, vec e, FILE *o) {
  fputc('[', o);
  if (e->len) for (mem i = e->xs, l = i + e->len;;) {
    emit(v, *i++, o);
    if (i < l) fputc(' ', o);
    else break; }
  fputc(']', o); }

static u0 emhomn(lips v, obj x, FILE *o) {
  fputc('\\', o);
  switch (kind(x)) {
    case Sym: return emsym(v, Y(x), o);
    case Two: if (symp(A(x))) emsym(v, Y(A(x)), o);
              if (twop(B(x))) emhomn(v, B(x), o); } }

u0 emit(lips v, obj x, FILE *o) {
  switch (kind(x)) {
    case Hom: return emhomn(v, homnom(v, x), o);
    case Num: return (u0) fprintf(o, "%ld", (long) N(x));
    case Sym: return emsym(v, getsym(x), o);
    case Two: return emtwo(v, gettwo(x), o);
    case Str: return emstr(v, getstr(x), o);
    case Tbl: return emtbl(v, gettbl(x), o);
    case Vec: return emvec(v, getvec(x), o);
    default:  fputs("()", o); } }

// print to console
Vm(em_u) {
  u64 l = N(Argc), i;
  if (l) {
    for (i = 0; i < l - 1; i++)
      emsep(v, Argv[i], stdout, ' ');
    emit(v, xp = Argv[i], stdout); }
  fputc('\n', stdout);
  Jump(ret); }

Vm(putc_u) {
  Ary(1);
  fputc(N(*Argv), stdout);
  Jump(ret); }

Vm(dump) {
  Ary(2);
  Tc(Argv[0], Str);
  Tc(Argv[1], Str);
  char *p = S(Argv[0])->text,
       *d = S(Argv[1])->text;
  write_file(v, p, d);
  Jump(ret); }
