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

NoInline const char* tnom(enum tag t) {
 switch (t) {
  case Hom: return "hom";
  case Num: return "num";
  case Tbl: return "tbl";
  case Two: return "two";
  case Tup: return "vec";
  case Oct: return "str";
  case Sym: return "sym";
  default:  return "nil"; } }

Ty O P(V, Io);
St P atom, r1s, qt, stri;

#define readx(v,m)(errp(v,m),0)

St int r0(Io i) { Fo (Z c;;) Sw ((c = getc(i))) {
 Ks '#': Ks ';': do c = getc(i); Wh (c != '\n' && c != EOF);
 Ks ' ': Ks '\t': Ks '\n': Cu;
 Df: R c; } }

O parse(V v, Io i) { Z c; Sw ((c = r0(i))) {
 Ks EOF:  R 0;
 Ks ')':  R readx(v, err_rpar);
 Ks '(':  R r1s(v, i);
 Ks '"':  R stri(v, i);
 Ks '\'': R qt(v, i);
 Df:      R ungetc(c, i), atom(v, i); } }

St O qt(V v, Io i) { O r;
 R !(r = parse(v, i)) ? r :
  (r = pair(v, r, nil),
   pair(v, Qt, r)); }

St O r1s(V v, Io i) { O x, y, c;
 R (c = r0(i)) == EOF ? readx(v, err_eof) :
  c == ')' ? nil :
   (ungetc(c, i),
    !(x = parse(v, i)) ? x :
     (Mm(x, y = r1s(v, i)),
      y ? pair(v, x, y) : y)); }

static obj
rloop(lips v, FILE *i, str o, i64 n, i64 lim,
      obj (*loop)(lips, FILE*, str, i64, i64)) {
 obj x;
 return
  o->len = n, x = putoct(o),
  o->text[n-1] == 0 ? x :
   (Mm(x, o = cells(v, 1 + b2w(2*n))),
    bcpy(o->text, getoct(x)->text, o->len = n),
    loop(v, i, o, n, 2 * n)); }

static obj
atom_(lips v, FILE *p, str o, i64 n, i64 lim) {
 obj x;
 while (n < lim) switch (x = fgetc(p)) {
  case ' ': case '\n': case '\t': case ';': case '#':
  case '(': case ')': case '\'': case '"':
   ungetc(x, p); case EOF:
   o->text[n++] = 0;
   goto out;
  default: o->text[n++] = x; } out:
 return rloop(v, p, o, n, lim, atom_); }

static obj
str_(lips v, FILE *p, str o, i64 n, i64 lim) {
 obj x;
 while (n < lim) switch (x = fgetc(p)) {
  case '\\': if ((x = fgetc(p)) == EOF) {
  case EOF: case '"': o->text[n++] = 0; goto out; }
  default: o->text[n++] = x; } out:
 return rloop(v, p, o, n, lim, str_); }

static Inline i64 chidx(char c, const char *s) {
 for (Z i = 0; *s; s++, i++) if (*s == c) return i;
 return -1; }

static NoInline obj readz_2(const char *s, i64 rad) {
  static const char *dig = "0123456789abcdef";
  if (!*s) return nil;
  i64 a = 0;
  int c;
  for (;;) {
    if (!(c = *s++)) break;
    a *= rad;
    int i = chidx(c, dig);
    if (i < 0 || i >= rad) return nil;
    a += i; }
  return Pn(a); }

static NoInline obj readz_1(const char *s) {
 if (*s == '0') switch (s[1]) {
  case 'b': return readz_2(s+2, 2);
  case 'o': return readz_2(s+2, 8);
  case 'd': return readz_2(s+2, 10);
  case 'z': return readz_2(s+2, 12);
  case 'x': return readz_2(s+2, 16); }
 return readz_2(s, 10); }

static Inline obj readz(const char *s) {
 if (*s == '-') {
  obj q = readz_1(s+1);
  return nump(q) ? Pn(-Gn(q)) : q; }
 if (*s == '+') R readz_1(s+1);
 return readz_1(s); }

static obj atom(lips v, FILE *i) {
 O o = atom_(v, i, cells(v, 2), 0, 8), q = readz(chars(o));
 return nump(q) ? q : intern(v, o); }

static obj stri(lips v, FILE *i) {
 return str_(v, i, cells(v, 2), 0, 8); }

u0 emsep(lips v, obj x, FILE *o, char s) {
 emit(v, x, o), fputc(s, o); }

static u0 emoct(lips v, str s, FILE *o) {
 fputc('"', o);
 for (Z i = 0, l = s->len - 1; i < l; i++)
  if (s->text[i] == '"') fputs("\\\"", o);
  else fputc(s->text[i], o);
 fputc('"', o); }

static u0 emtbl(lips v, tbl t, FILE *o) {
 fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

static u0 emsym(lips v, sym y, FILE *o) {
 nilp(y->nom) ?
  fprintf(o, "#sym@%lx", (Z) y) :
  fputs(chars(y->nom), o); }

static u0 emtwo_(lips v, two w, FILE *o) {
 twop(w->y) ?
  (emsep(v, w->x, o, ' '), emtwo_(v, gettwo(w->y), o)) :
  emsep(v, w->x, o, ')'); }

static u0 emtwo(lips v, two w, FILE *o) {
 w->x == Qt && twop(w->y) && nilp(Y(w->y)) ?
  (fputc('\'', o), emit(v, X(w->y), o)) :
  (fputc('(', o), emtwo_(v, w, o)); }

static u0 emnum(lips v, i64 n, FILE *o) {
  fprintf(o, "%ld", n); }

static u0 phomn(lips v, obj x, FILE *o) {
 fputc('\\', o); 
 switch (kind(x)) {
  case Sym: emit(v, x, o); break;
  case Two:
   if (symp(X(x))) emit(v, X(x), o);
   if (twop(Y(x))) phomn(v, Y(x), o); } }

static u0 emhom(lips v, hom h, FILE *o) {
 phomn(v, homnom(v, Ph(h)), o); }

u0 emit(lips v, obj x, FILE *o) {
 switch (kind(x)) {
  Ks Hom:  return emhom(v, Gh(x), o);
  Ks Num:  return emnum(v, Gn(x), o);
  Ks Sym:  return emsym(v, getsym(x), o);
  Ks Two:  return emtwo(v, gettwo(x), o);
  Ks Oct:  return emoct(v, getoct(x), o);
  Ks Tbl:  return emtbl(v, gettbl(x), o);
  default: return (u0) fputs("()", o); } }

u0 errp(lips v, const char *msg, ...) {
 va_list xs;
 fputs("# ", stderr);
 va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
 fputc('\n', stderr); }
