#include "lips.h"
////
/// lisp parser
//
// this should be portable to lisp as soon as
// the string processing primitives are good
// enough, at which point it can be called the
// bootstrap parser
#define err_eof "unexpected eof"
#define err_rpar "unmatched right delimiter"

typedef obj P(vm, FILE*);
static P atom, reads, quote, str;

#define readx(v,m)err(v,"parse",0,m)

static int read0(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case ';': do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: return c; } }

obj parse(vm v, FILE *i) {
  for (int c;;) switch ((c = read0(i))) {
    case EOF:  return 0;
    case ')':  return readx(v, err_rpar);
    case '(':  return reads(v, i);
    case '"':  return str(v, i);
    case '\'': return quote(v, i);
    default:   return ungetc(c, i), atom(v, i); } }

static obj quote(vm v, FILE *i) {
  obj r = parse(v, i);
  if (r) return (r = pair(v, r, nil), pair(v, Qt, r));
  return feof(i) ? 0 : readx(v, err_eof); }

static obj reads(vm v, FILE *i) {
  obj x, y, c;
  switch ((c = read0(i))) {
    case EOF: return readx(v, err_eof);
    case ')': return nil;
    default:  ungetc(c, i);
              if (!(x = parse(v, i))) return x;
              with(x, y = reads(v, i));
              return y ? pair(v, x, y) : y; } }

static obj rloop(vm v, FILE *i, oct o, num n, num lim,
  obj (*re)(vm, FILE*, oct, num, num)) {
  obj x;
  o->len = n, x = putoct(o);
  return o->text[n-1] == 0 ? x :
    (with(x, o = cells(v, 1 + b2w(2*n))),
     memcpy(o->text, getoct(x)->text, o->len = n),
     re(v, i, o, n, 2 * n)); }

static obj atom_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case ' ': case '\n': case '\t': case ';':
    case '(': case ')': case '\'': case '"':
      ungetc(x, p); case EOF:
      o->text[n++] = 0; goto out;
    default: o->text[n++] = x; } out:
  return rloop(v, p, o, n, lim, atom_); }

static obj str_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case '\\': if ((x = fgetc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0; goto out; }
    default: o->text[n++] = x; } out:
  return rloop(v, p, o, n, lim, str_); }

static obj slurp_(vm v, FILE *i, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(i)) {
    case EOF: o->text[n++] = 0; goto out;
    default: o->text[n++] = x; } out:
  return rloop(v, i, o, n, lim, slurp_); }

static obj atom(vm v, FILE *i) {
  obj o = atom_(v, i, cells(v, 2), 0, 8);
  char *st = NULL;
  num j = strtol(chars(o), &st, 0);
  return !st || *st ? intern(v, o) : putnum(j); }

obj slurp(vm v, FILE *i) {
  return slurp_(v, i, cells(v, 3), 0, 16); }

static obj str(vm v, FILE *i) {
  return str_(v, i, cells(v, 2), 0, 8); }

void emsep(vm v, obj x, FILE *o, char s) {
  emit(v, x, o), fputc(s, o); }

static void emit_2(vm v, obj x, FILE *o) {
  twop(Y(x)) ? (emsep(v, X(x), o, ' '), emit_2(v, Y(x), o)) :
                emsep(v, X(x), o, ')'); }

static void emhom(vm v, obj x, FILE *o) {
  x = homnom(v, x);
  fputc('\\', o);
  if (symp(x)) fputs(symnom(x), o); }

static void emoct(vm v, obj x, FILE *o) {
  fputc('"', o);
  oct s = getoct(x);
  for (num i = 0, l = s->len - 1; i < l; i++)
    if (s->text[i] == '"') fputs("\\\"", o);
    else fputc(s->text[i], o);
  fputc('"', o); }
static void emtbl(vm v, obj x, FILE *o) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

void emit(vm v, obj x, FILE *o) {
  switch (kind(x)) {
    case Num: fprintf(o, "%ld", getnum(x)); break;
    case Sym: fputs(symnom(x), o); break;
    case Hom: return emhom(v, x, o);
    case Two: fputc('(', o); emit_2(v, x, o); break;
    case Oct: return emoct(v, x, o);
    case Tbl: return emtbl(v, x, o);
    default: fputs("()", o); } }
