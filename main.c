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

Nin Ko Ch* tnom(En type t) { Sw (t) {
 Ks Hom: R "hom";
 Ks Num: R "num";
 Ks Tbl: R "tbl";
 Ks Two: R "two";
 Ks Tup: R "vec";
 Ks Oct: R "str";
 Ks Sym: R "sym";
 Df:  R "nil"; } }

Ty O P(vm, FILE*);
St P atom, r1s, qt, str;

#define readx(v,m)(errp(v,m),0)

St int r0(Io i) { Fo (Z c;;) Sw ((c = getc(i))) {
 Ks '#': Ks ';': do c = getc(i); Wh (c != '\n' && c != EOF);
 Ks ' ': Ks '\t': Ks '\n': Cu;
 Df: R c; } }

O parse(vm v, Io i) { Z c; Sw ((c = r0(i))) {
 Ks EOF:  R 0;
 Ks ')':  R readx(v, err_rpar);
 Ks '(':  R r1s(v, i);
 Ks '"':  R str(v, i);
 Ks '\'': R qt(v, i);
 Df:      R ungetc(c, i), atom(v, i); } }

St O qt(vm v, FILE *i) { O r;
 R !(r = parse(v, i)) ? r :
  (r = pair(v, r, nil),
   pair(v, Qt, r)); }

St O r1s(vm v, FILE *i) { O x, y, c;
 R (c = r0(i)) == EOF ? readx(v, err_eof) :
  c == ')' ? nil :
   (ungetc(c, i),
    !(x = parse(v, i)) ? x :
     (Mm(x, y = r1s(v, i)),
      y ? pair(v, x, y) : y)); }

St O rloop(V v, Io i, By o, Z n, Z lim,
           O (*re)(V, Io, By, Z, Z)) { O x;
 R o->len = n, x = putoct(o),
  o->text[n-1] == 0 ? x :
   (Mm(x, o = cells(v, 1 + b2w(2*n))),
    memcpy(o->text, getoct(x)->text, o->len = n),
    re(v, i, o, n, 2 * n)); }

St O atom_(V v, Io p, By o, Z n, Z lim) { O x;
 Wh (n < lim) Sw (x = fgetc(p)) {
  Ks ' ': Ks '\n': Ks '\t': Ks ';': Ks '#':
  Ks '(': Ks ')': Ks '\'': Ks '"':
   ungetc(x, p); Ks EOF:
   o->text[n++] = 0;
   goto out;
  Df: o->text[n++] = x; } out:
 R rloop(v, p, o, n, lim, atom_); }

St O str_(V v, Io p, By o, Z n, Z lim) { O x;
 Wh (n < lim) Sw (x = fgetc(p)) {
  Ks '\\': if ((x = fgetc(p)) == EOF) {
  Ks EOF: Ks '"': o->text[n++] = 0; goto out; }
  Df: o->text[n++] = x; } out:
 R rloop(v, p, o, n, lim, str_); }

St O atom(V v, Io i) {
 O o = atom_(v, i, cells(v, 2), 0, 8);
 Ch *st = NULL;
 Z j = strtol(chars(o), &st, 0);
 R !st || *st ? intern(v, o) : putnum(j); }

St O str(V v, Io i) {
 R str_(v, i, cells(v, 2), 0, 8); }

_ emsep(V v, O x, Io o, Ch s) {
 emit(v, x, o), fputc(s, o); }

St _ phomn(V v, O x, Io o) {
 fputc('\\', o); 
 Sw (kind(x)) {
  Ks Sym: emit(v, x, o); Bk;
  Ks Two:
   if (symp(X(x))) emit(v, X(x), o);
   if (twop(Y(x))) phomn(v, Y(x), o); } }

St _ emoct(V v, By s, Io o) {
 fputc('"', o);
 Fo (num i = 0, l = s->len - 1; i < l; i++)
  if (s->text[i] == '"') fputs("\\\"", o);
  El fputc(s->text[i], o);
 fputc('"', o); }

St _ emtbl(V v, Ht t, Io o) {
 fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

St _ emsym(V v, Sy y, Io o) {
 nilp(y->nom) ?
  fprintf(o, "#sym@%lx", (num) y) :
  fputs(chars(y->nom), o); }

St _ emtwo_(V v, Tw w, Io o) {
 twop(w->y) ?
  (emsep(v, w->x, o, ' '), emtwo_(v, gettwo(w->y), o)) :
  emsep(v, w->x, o, ')'); }

St _ emtwo(V v, Tw w, Io o) {
 w->x == Qt && twop(w->y) && nilp(Y(w->y)) ?
  (fputc('\'', o), emit(v, X(w->y), o)) :
  (fputc('(', o), emtwo_(v, w, o)); }

St _ emnum(V v, Z n, Io o) {
 fprintf(o, "%ld", n); }

St _ emhom(V v, H h, Io o) {
 phomn(v, homnom(v, Ph(h)), o); }

_ emit(V v, O x, Io o) { Sw (kind(x)) {
 Ks Hom: R emhom(v, Gh(x), o);
 Ks Num: R emnum(v, Gn(x), o);
 Ks Sym: R emsym(v, getsym(x), o);
 Ks Two: R emtwo(v, gettwo(x), o);
 Ks Oct: R emoct(v, getoct(x), o);
 Ks Tbl: R emtbl(v, gettbl(x), o);
 Df:  R (_) fputs("()", o); } }

_ errp(V v, Ko Ch *msg, ...) { va_list xs;
 fputs("# ", stderr);
 va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
 fputc('\n', stderr); }
#define OK EXIT_SUCCESS
#define NO EXIT_FAILURE

St int repl(rt v, FILE *i, FILE *o) {
  obj x;
  for (setjmp(v->restart);;)
    if ((x = parse(v, i))) emsep(v, eval(v, x), o, '\n');
    else if (feof(i)) break;
  return OK; }

_ scr(vm v, FILE *f) {
  for (obj x; (x = parse(v, f)); eval(v, x)); }

St int scripts(rt v, char**argv) {
 Fo (char *q; (q = *argv++);) {
  Io f = fopen(q, "r");
  if (!f) R errp(v, "%s : %s", q, strerror(errno)), NO;
  if (setjmp(v->restart)) R
   errp(v, "%s : fail", q),
   fclose(f),
   NO;
  scr(v, f);
  int ok = feof(f);
  fclose(f);
  if (!ok) R NO; }
 R OK; }

int main(int argc, char**argv) {
#define takka 1
#define nprel 2
 int opt, args,
  F = argc == 1 ? takka : 0;
 const char
  opts[] = "hi_",
  help[] =
   "usage: %s [options and scripts]\n"
   "options:\n"
   "  -_ don't bootstrap\n"
   "  -i interact unconditionally\n"
   "  -h print this message\n";
 Wh ((opt = getopt(argc, argv, opts)) != -1) Sw (opt) {
  Ks '?': R NO;
  Ks '_': F|=nprel; Bk;
  Ks 'i': F|=takka; Bk;
  Ks 'h': fprintf(stdout, help, argv[0]); Bk; }

 args = argc - optind;
 if (args == 0 && !(F&takka)) R OK;

 V v = initialize(argc, (Ko Ch**) argv);
 v = F&nprel ? v : bootstrap(v);
 if (!v) R NO;

 int r = OK;
 if (args) r = scripts(v, argv + optind);
 if (r == OK && F&takka) repl(v, stdin, stdout);
 R finalize(v), r; }
