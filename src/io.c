#include "lips.h"
#include "io.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "str.h"
#include "err.h"
#include "terp.h"
#include "hom.h"
#include <string.h>
#include <errno.h>

#define bind(v, x) if (!((v)=(x))) return 0

// these are the names of the fundamental types.
// obviously this only works if the type names
// are all 4 bytes long (counting the NUL)
const uint32_t *tnoms = (uint32_t*)
 "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";

////
/// lisp parser
//
// this should be portable to lisp as soon as
// the string processing primitives are good
// enough, at which point it can be called the
// bootstrap parser

typedef obj read_loop(lips, FILE*, str, u64, u64);
static obj read_list(lips, FILE*), readz(lips, const char*);
static read_loop read_str, read_atom;

static Inline obj read_buffered(lips v, FILE *i, read_loop *loop) {
  str c = cells(v, 2);
  return loop(v, i, c, 0, c->len = 8); }

static int read_char(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case '#': case ';':
      do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: return c; } }

obj read_quoted(lips v, FILE *i) {
 obj x;
 bind(x, parse(v, i));
 x = pair(v, x, nil);
 return pair(v, Qt, x); }

obj parse(lips v, FILE* i) {
 int c = read_char(i);
 obj x, y;
 switch (c) {
  case EOF: return 0;
  case ')': return errp(v, "unmatched %s delimiter", "right"), 0;
  case '(': return read_list(v, i);
  case '"': return read_buffered(v, i, read_str);
  case '\'': return read_quoted(v, i);
  default: return
   ungetc(c, i),
   x = read_buffered(v, i, read_atom),
   y = readz(v, chars(x)),
   nump(y) ? y : intern(v, x); } }

VM(par_u) {
  PACK();
  obj x = parse(v, stdin);
  if (!x && !feof(stdin)) return restart(v);
  else v->xp = x ? pair(v, x, nil) : nil;
  UNPACK();
  Jump(ret); }

static obj read_list(lips v, FILE *i) {
 obj x, y, c = read_char(i);
 switch (c) {
  case EOF: return errp(v, "unmatched %s delimiter", "left"), 0;
  case ')': return nil;
  default: return
   ungetc(c, i),
   x = parse(v, i),
   with(x, y = read_list(v, i)),
   pair(v, x, y); } }

static obj grow_buffer(lips v, obj s) {
  num l = b2w(S(s)->len);
  obj t;
  with(s, t = putstr(cells(v, 2*l+1)));
  S(t)->len = w2b(2*l);
  cpy64(S(t)->text, S(s)->text, l);
  return t; }

static obj read_atom(lips v, FILE *p, str o, u64 n, u64 lim) {
 for (obj x; n < lim;) switch (x = getc(p)) {
  case ' ': case '\n': case '\t': case ';': case '#':
  case '(': case ')': case '\'': case '"':
   ungetc(x, p); case EOF:
   o->text[n++] = 0, o->len = n;
   return putstr(o);
  default: o->text[n++] = x; }
 return read_atom(v, p, S(grow_buffer(v, _S(o))), lim, 2*lim); }

static obj read_str(lips v, FILE *p, str o, u64 n, u64 lim) {
 for (obj x; n < lim;) switch (x = getc(p)) {
  case '\\': if ((x = getc(p)) == EOF) {
  case EOF: case '"': o->text[n++] = 0, o->len = n ;return _S(o); }
  default: o->text[n++] = x; }
 return read_str(v, p, S(grow_buffer(v, _S(o))), lim, 2*lim); }

static obj read_file_loop(lips v, FILE *p, str o, u64 n, u64 lim) {
 for (obj x; n < lim;) switch (x = getc(p)) {
  case EOF: o->text[n++] = 0, o->len = n; return _S(o);
  default: o->text[n++] = x; }
 return read_file_loop(v, p, S(grow_buffer(v, _S(o))), lim, 2*lim); }

obj read_file(lips v, const char *path) {
 FILE *i = fopen(path, "r");
 if (!i) return errp(v, "%s : %s", path, strerror(errno)), restart(v);
 obj s = read_buffered(v, i, read_file_loop);
 fclose(i);
 return s; }

obj write_file(lips v, const char *path, const char *text) {
 FILE *out = fopen(path, "w");
 if (!out) return
  errp(v, "%s : %s", path, strerror(errno)),
  restart(v);
 for (int c = *text; c; c = *++text) fputc(c, out);
 fclose(out);
 return nil; }

static NoInline obj readz_2(const char *s, i64 rad) {
 static const char *dig = "0123456789abcdef";
 if (!*s) return nil;
 i64 a = 0;
 for (int i, c; (c = *s++); a += i) {
  a *= rad, i = sidx(dig, cmin(c));
  if (i < 0 || i >= rad) return nil; }
 return _N(a); }

static NoInline obj readz_1(const char *s) {
 if (*s == '0') switch (cmin(s[1])) {
  case 'b': return readz_2(s+2, 2);
  case 'o': return readz_2(s+2, 8);
  case 'd': return readz_2(s+2, 10);
  case 'z': return readz_2(s+2, 12);
  case 'x': return readz_2(s+2, 16); }
 return readz_2(s, 10); }

static Inline obj readz(lips _, const char *s) {
 obj q;
 switch (*s) {
  case '-': return nump(q = readz_1(s+1)) ? _N(-N(q)) : q;
  case '+': s++;
  default: return readz_1(s); } }


u0 ems(lips v, FILE *o, obj x, char s) {
 emit(v, x, o), fputc(s, o); }
u0 emsep(lips v, obj x, FILE *o, char s) {
 emit(v, x, o), fputc(s, o); }

static u0 emstr(lips v, str s, FILE *o) {
 fputc('"', o);
 for (char *t = s->text; *t; fputc(*t++, o)) if (*t == '"') fputc('\\', o);
 fputc('"', o); }

static u0 emtbl(lips v, tbl t, FILE *o) {
 fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }

static u0 emsym(lips v, sym y, FILE *o) {
 y->nom == nil ? fprintf(o, "#sym@%lx", (long) y) :
                 fputs(chars(y->nom), o); }

static u0 emtwo_(lips v, two w, FILE *o) {
 twop(w->b) ? (emsep(v, w->a, o, ' '), emtwo_(v, gettwo(w->b), o)) :
              emsep(v, w->a, o, ')'); }

static u0 emtwo(lips v, two w, FILE *o) {
 w->a == Qt && twop(w->b) && nilp(B(w->b)) ?
  (fputc('\'', o), emit(v, A(w->b), o)) :
  (fputc('(', o), emtwo_(v, w, o)); }

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
  case Sym: return emit(v, x, o);
  case Two: if (symp(A(x))) emit(v, A(x), o);
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
VM(em_u) {
 u64 l = N(ARGC), i;
 if (l) {
  for (i = 0; i < l - 1; i++)
   emsep(v, ARGV[i], stdout, ' ');
  emit(v, xp = ARGV[i], stdout); }
 fputc('\n', stdout);
 Jump(ret); }

VM(putc_u) { ARY(1); fputc(N(*ARGV), stdout); Jump(ret); }
VM(getc_u) { GO(ret, feof(stdin) ? nil : _N(getc(stdin))); }

VM(slurp) {
  ARY(1);
  xp = *ARGV;
  TC(xp, Str);
  RETC(v->xp = read_file(v, getstr(xp)->text)); }
VM(dump) {
  ARY(2);
  TC(ARGV[0], Str); TC(ARGV[1], Str);
  char *p = S(ARGV[0])->text,
       *d = S(ARGV[1])->text;
  GO(ret, write_file(v, p, d)); }
