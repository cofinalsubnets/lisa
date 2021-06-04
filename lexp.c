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

typedef obj par(lips, FILE*);
static par atom, r1s, qt, stri;

#define readx(v,m)(errp(v,m),0)

static int r0(FILE *i) {
 for (int c;;) switch ((c = getc(i))) {
  case '#': case ';':
   do c = getc(i); while (c != '\n' && c != EOF);
  case ' ': case '\t': case '\n': continue;
  default: return c; } }

obj parse(lips v, FILE* i) {
 int c;
 switch ((c = r0(i))) {
  case EOF:  return 0;
  case ')':  return readx(v, err_rpar);
  case '(':  return r1s(v, i);
  case '"':  return stri(v, i);
  case '\'': return qt(v, i);
  default:   return ungetc(c, i), atom(v, i); } }

static obj qt(lips v, FILE *i) {
 obj r;
 return !(r = parse(v, i)) ? r :
  (r = pair(v, r, nil),
   pair(v, Qt, r)); }

static obj r1s(lips v, FILE *i) {
 obj x, y, c;
 return (c = r0(i)) == EOF ? readx(v, err_eof) :
  c == ')' ? nil :
   (ungetc(c, i),
    !(x = parse(v, i)) ? x :
     (Mm(x, y = r1s(v, i)),
      y ? pair(v, x, y) : y)); }

static NoInline obj
rloop(lips v, FILE *i, str o, i64 n, i64 lim,
      obj (*loop)(lips, FILE*, str, i64, i64)) {
 obj x;
 return
  o->len = n, x = putoct(o),
  o->text[n-1] == 0 ? x :
   (Mm(x, o = cells(v, 1 + b2w(2*n))),
    cpy8(o->text, getoct(x)->text, o->len = n),
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

static NoInline obj readz_2(const char *s, i64 rad) {
 static const char *dig = "0123456789abcdef";
 if (!*s) return nil;
 i64 a = 0;
 for (int i, c; (c = *s++); a += i) {
  a *= rad;
  i = sidx(dig, cmin(c));
  if (i < 0 || i >= rad) return nil; }
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
 if (*s == '+') return readz_1(s+1);
 return readz_1(s); }

static obj atom(lips v, FILE *i) {
 obj o = atom_(v, i, cells(v, 2), 0, 8), q = readz(chars(o));
 return nump(q) ? q : intern(v, o); }

static obj stri(lips v, FILE *i) {
 return str_(v, i, cells(v, 2), 0, 8); }

u0 emsep(lips v, obj x, FILE *o, char s) {
 emit(v, x, o), fputc(s, o); }

static u0 emoct(lips v, str s, FILE *o) {
 fputc('"', o);
 for (i64 i = 0, l = s->len - 1; i < l; i++)
  if (s->text[i] == '"') fputs("\\\"", o);
  else fputc(s->text[i], o);
 fputc('"', o); }

static u0 emtbl(lips v, tbl t, FILE *o) {
 fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

static u0 emsym(lips v, sym y, FILE *o) {
 nilp(y->nom) ?
  fprintf(o, "#sym@%lx", (u64) y) :
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
  case Hom:  return emhom(v, Gh(x), o);
  case Num:  return emnum(v, Gn(x), o);
  case Sym:  return emsym(v, getsym(x), o);
  case Two:  return emtwo(v, gettwo(x), o);
  case Oct:  return emoct(v, getoct(x), o);
  case Tbl:  return emtbl(v, gettbl(x), o);
  default: return (u0) fputs("()", o); } }

u0 errp(lips v, char *msg, ...) {
 va_list xs;
 fputs("# ", stderr);
 va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
 fputc('\n', stderr); }
