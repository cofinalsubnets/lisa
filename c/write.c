#include "lips.h"
#include "write.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "str.h"
#include "terp.h"
#include "tbl.h"
#include "hom.h"
#include "vec.h"

typedef u0 writer(lips, obj, FILE*);
static writer
  write_nil, write_two, write_num, write_vec,
  write_str, write_sym, write_tbl, write_hom;
static writer *writers[] = {
  [Hom] = write_hom,
  [Num] = write_num,
  [Tbl] = write_tbl,
  [Nil] = write_nil,
  [Str] = write_str,
  [Vec] = write_vec,
  [Sym] = write_sym,
  [Two] = write_two, };

static u0 write_nil(lips v, obj x, FILE *o) {
  fputs("()", o); }

static u0 write_num(lips v, obj x, FILE *o) {
  fprintf(o, "%ld", (long) N(x)); }

static u0 write_sym(lips v, obj x, FILE *o) {
  sym y = Y(x);
  nilp(y->nom) ? fprintf(o, "#sym@%lx", (long) y) :
                 fputs(S(y->nom)->text, o); }

static u0 write_vec(lips v, obj x, FILE *o) {
  vec e = V(x);
  fputc('[', o);
  if (e->len) for (mem i = e->xs, l = i + e->len;;) {
    emit(v, *i++, o);
    if (i < l) fputc(' ', o);
    else break; }
  fputc(']', o); }

static u0 emhomn(lips v, obj x, FILE *o) {
  fputc('\\', o);
  switch (kind(x)) {
    case Sym: return write_sym(v, x, o);
    case Two: if (symp(A(x))) write_sym(v, A(x), o);
              emhomn(v, B(x), o); } }

static u0 write_hom(lips v, obj x, FILE *o) {
  emhomn(v, homnom(v, x), o); }

static u0 write_tbl(lips v, obj x, FILE *o) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }

static u0 write_str(lips v, obj x, FILE *o) {
  str s = S(x);
  fputc('"', o);
  for (char *t = s->text; *t; fputc(*t++, o))
    if (*t == '"') fputc('\\', o);
  fputc('"', o); }

static u0 write_two_(lips v, two w, FILE *o) {
  twop(w->b) ? (emsep(v, w->a, o, ' '),
                write_two_(v, gettwo(w->b), o)) :
               emsep(v, w->a, o, ')'); }

static Inline u1 quotate(lips v, two w) {
  return w->a == Qt && twop(w->b) && nilp(B(w->b)); }

static u0 write_two(lips v, obj x, FILE *o) {
  if (quotate(v, gettwo(x))) fputc('\'', o), emit(v, A(B(x)), o);
  else fputc('(', o), write_two_(v, gettwo(x), o); }

Inline u0 emit(lips v, obj x, FILE *o) {
  writers[kind(x)](v, x, o); }

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
  write_file(v, p, d); // FIXME handle failure
  Jump(ret); }

u1 write_file(lips v, const char *path, const char *text) {
  FILE *out;
  bind(out, fopen(path, "w"));
  u1 r = true;
  for (char c = *text; r && c; c = *++text)
    r = fputc(c, out) != EOF;
  fclose(out);
  return r; }
