#include "lips.h"
#include "terp.h"

typedef u0 writer(en, ob, FILE*);
static writer nil_out, two_out, num_out, vec_out,
              str_out, sym_out, tbl_out, hom_out;
static writer *writers[] = {
  [Hom] = hom_out, [Num] = num_out, [Tbl] = tbl_out, [Nil] = nil_out,
  [Str] = str_out, [Vec] = vec_out, [Sym] = sym_out, [Two] = two_out, };

static u0 nil_out(en v, ob x, FILE *o) {
  fputs("()", o); }

static u0 num_out(en v, ob x, FILE *o) {
  fprintf(o, "%ld", (long) N(x)); }

static u0 sym_out(en v, ob x, FILE *o) {
  sym y = Y(x);
  nilp(y->nom) ? fprintf(o, "#sym@%lx", (long) y) :
                 fputs(S(y->nom)->text, o); }

static u0 vec_out(en v, ob x, FILE *o) {
  vec e = V(x);
  fputc('[', o);
  if (e->len) for (ob*i = e->xs, *l = i + e->len;;) {
    emit(v, *i++, o);
    if (i < l) fputc(' ', o);
    else break; }
  fputc(']', o); }

static u0 emhomn(en v, ob x, FILE *o) {
  fputc('\\', o);
  switch (kind(x)) {
    case Sym: return sym_out(v, x, o);
    case Two: if (symp(A(x))) sym_out(v, A(x), o);
              emhomn(v, B(x), o); } }

static u0 hom_out(en v, ob x, FILE *o) {
  emhomn(v, homnom(v, x), o); }

static u0 tbl_out(en v, ob x, FILE *o) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }

static u0 str_out(en v, ob x, FILE *o) {
  str s = S(x);
  fputc('"', o);
  for (char *t = s->text; *t; fputc(*t++, o))
    if (*t == '"') fputc('\\', o);
  fputc('"', o); }

static u0 two_out_(en v, two w, FILE *o) {
  twop(w->b) ? (emsep(v, w->a, o, ' '),
                two_out_(v, gettwo(w->b), o)) :
               emsep(v, w->a, o, ')'); }

SI u1 quotate(en v, two w) {
  return w->a == Qt && twop(w->b) && nilp(B(w->b)); }

static u0 two_out(en v, ob x, FILE *o) {
  if (quotate(v, gettwo(x))) fputc('\'', o), emit(v, A(B(x)), o);
  else fputc('(', o), two_out_(v, gettwo(x), o); }

Inline u0 emit(en v, ob x, FILE *o) {
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

u1 write_file(en v, const char *path, const char *text) {
  FILE *out;
  bind(out, fopen(path, "w"));
  u1 r = true;
  for (char c = *text; r && c; c = *++text)
    r = fputc(c, out) != EOF;
  fclose(out);
  return r; }
