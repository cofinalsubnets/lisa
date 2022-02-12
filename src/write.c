#include "lips.h"
#include "write.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "str.h"
#include "terp.h"

u0 ems(lips v, FILE *o, obj x, char s) {
 emit(v, o, x), fputc(s, o); }

u0 emsep(lips v, obj x, FILE *o, char s) {
 emit(v, o, x), fputc(s, o); }

#include "tbl.h"
#include "hom.h"
#include "vec.h"
static u0 emnil(lips v, FILE *o, obj x) { fputs("()", o); }

u0 emnum(lips v, FILE *o, obj x) {
  fprintf(o, "%ld", (long) N(x)); }

u0 emsym(lips v, FILE *o, obj x) {
  sym y = Y(x);
  y->nom == nil ? fprintf(o, "#sym@%lx", (long) y) :
                  fputs(S(y->nom)->text, o); }

u0 emvec(lips v, FILE *o, obj x) {
  vec e = V(x);
  fputc('[', o);
  if (e->len) for (mem i = e->xs, l = i + e->len;;) {
    emit(v, o, *i++);
    if (i < l) fputc(' ', o);
    else break; }
  fputc(']', o); }

static u0 emhomn(lips v, FILE *o, obj x) {
  fputc('\\', o);
  switch (kind(x)) {
    case Sym: return emsym(v, o, x);
    case Two: if (symp(A(x))) emsym(v, o, A(x));
              if (twop(B(x))) emhomn(v, o, B(x)); } }

u0 emhom(lips v, FILE *o, obj x) {
  emhomn(v, o, homnom(v, x)); }

u0 emtbl(lips v, FILE *o, obj x) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }

u0 emstr(lips v, FILE *o, obj x) {
  str s = S(x);
  fputc('"', o);
  for (char *t = s->text; *t; fputc(*t++, o))
    if (*t == '"') fputc('\\', o);
  fputc('"', o); }

static u0 emtwo_(lips v, FILE *o, two w) {
  twop(w->b) ? (emsep(v, w->a, o, ' '),
                emtwo_(v, o, gettwo(w->b))) :
               emsep(v, w->a, o, ')'); }

static Inline u1 quotate(lips v, two w) {
  return w->a == Qt && twop(w->b) && nilp(B(w->b)); }

u0 emtwo(lips v, FILE *o, obj x) {
  if (quotate(v, gettwo(x)))
    fputc('\'', o), emit(v, o, A(B(x)));
  else fputc('(', o), emtwo_(v, o, gettwo(x)); }

static emitter *emitters[] = {
  [Hom] = emhom,
  [Tbl] = emtbl,
  [Num] = emnum,
  [Str] = emstr,
  [Sym] = emsym,
  [Nil] = emnil,
  [Vec] = emvec,
  [Two] = emtwo, };

Inline u0 emit(lips v, FILE *o, obj x) {
  emitters[kind(x)](v, o, x); }

// print to console
Vm(em_u) {
  u64 l = N(Argc), i;
  if (l) {
    for (i = 0; i < l - 1; i++)
      emsep(v, Argv[i], stdout, ' ');
    emit(v, stdout, xp = Argv[i]); }
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
