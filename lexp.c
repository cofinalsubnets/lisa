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

Nin Ko char *tnom(enum type t) { switch (t) {
  case Hom: R "hom";
  case Num: R "num";
  case Tbl: R "tbl";
  case Two: R "two";
  case Tup: R "vec";
  case Oct: R "str";
  case Sym: R "sym";
  default:  R "nil"; } }

Ty obj P(vm, FILE*);
St P atom, reads, quote, str;

#define readx(v,m)(errp(v,m),0)

St int read0(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case '#':
    case ';': do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: R c; } }

obj parse(vm v, FILE *i) {
  for (int c;;) switch ((c = read0(i))) {
    case EOF:  R 0;
    case ')':  R readx(v, err_rpar);
    case '(':  R reads(v, i);
    case '"':  R str(v, i);
    case '\'': R quote(v, i);
    default:   R ungetc(c, i), atom(v, i); } }

St obj quote(vm v, FILE *i) {
  obj r = parse(v, i);
  R !r ? r : (r = pair(v, r, nil), pair(v, Qt, r)); }

St obj reads(vm v, FILE *i) {
  obj x, y, c;
  switch ((c = read0(i))) {
    case EOF: R readx(v, err_eof);
    case ')': R nil;
    default:  ungetc(c, i);
              if (!(x = parse(v, i))) R x;
              with(x, y = reads(v, i));
              R y ? pair(v, x, y) : y; } }

St obj rloop(vm v, FILE *i, oct o, num n, num lim,
  obj (*re)(vm, FILE*, oct, num, num)) {
  obj x;
  o->len = n, x = putoct(o);
  R o->text[n-1] == 0 ? x :
    (with(x, o = cells(v, 1 + b2w(2*n))),
     memcpy(o->text, getoct(x)->text, o->len = n),
     re(v, i, o, n, 2 * n)); }

St obj atom_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case ' ': case '\n': case '\t': case ';': case '#':
    case '(': case ')': case '\'': case '"':
      ungetc(x, p); case EOF:
      o->text[n++] = 0;
      goto out;
    default: o->text[n++] = x; } out:
  R rloop(v, p, o, n, lim, atom_); }

St obj str_(vm v, FILE *p, oct o, num n, num lim) {
  obj x;
  while (n < lim) switch (x = fgetc(p)) {
    case '\\': if ((x = fgetc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0; goto out; }
    default: o->text[n++] = x; } out:
  R rloop(v, p, o, n, lim, str_); }

St obj atom(vm v, FILE *i) {
  obj o = atom_(v, i, cells(v, 2), 0, 8);
  char *st = NULL;
  num j = strtol(chars(o), &st, 0);
  R !st || *st ? intern(v, o) : putnum(j); }

St obj str(vm v, FILE *i) {
  R str_(v, i, cells(v, 2), 0, 8); }

__ emsep(vm v, obj x, FILE *o, char s) {
  emit(v, x, o), fputc(s, o); }

St __ phomn(vm v, obj x, FILE *o) {
  fputc('\\', o); 
  switch (kind(x)) {
    case Sym: emit(v, x, o); Bk;
    case Two:
      if (symp(X(x))) emit(v, X(x), o);
      if (twop(Y(x))) phomn(v, Y(x), o); } }

St __ emoct(vm v, oct s, FILE *o) {
  fputc('"', o);
  for (num i = 0, l = s->len - 1; i < l; i++)
    if (s->text[i] == '"') fputs("\\\"", o);
    else fputc(s->text[i], o);
  fputc('"', o); }
St __ emtbl(vm v, tbl t, FILE *o) {
  fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }
St __ emsym(vm v, sym y, FILE *o) {
  nilp(y->nom) ? fprintf(o, "#sym@%lx", (num) y) :
                 fputs(chars(y->nom), o); }
St __ emtwo_(vm v, two w, FILE *o) {
  twop(w->y) ? (emsep(v, w->x, o, ' '), emtwo_(v, gettwo(w->y), o)) :
                emsep(v, w->x, o, ')'); }
St __ emtwo(vm v, two w, FILE *o) {
  if (w->x == Qt && twop(w->y) && nilp(Y(w->y)))
    fputc('\'', o), emit(v, X(w->y), o);
  else fputc('(', o), emtwo_(v, w, o); }
St __ emnum(vm v, num n, FILE *o) {
  fprintf(o, "%ld", n); }
St __ emhom(vm v, hom h, FILE *o) {
  phomn(v, homnom(v, Ph(h)), o); }

void emit(vm v, obj x, FILE *o) {
  switch (kind(x)) {
    case Hom: R emhom(v, Gh(x), o);
    case Num: R emnum(v, Gn(x), o);
    case Sym: R emsym(v, getsym(x), o);
    case Two: R emtwo(v, gettwo(x), o);
    case Oct: R emoct(v, getoct(x), o);
    case Tbl: R emtbl(v, gettbl(x), o);
    default:  R (__) fputs("()", o); } }

void errp(vm v, const char *msg, ...) {
  va_list xs;
  fputs("# ", stderr);
  va_start(xs, msg),
  vfprintf(stderr, msg, xs);
  va_end(xs);
  fputc('\n', stderr); }
