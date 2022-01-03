#include "lips.h"
// these are the names of the fundamental types.
// obviously this only works if the type names
// are all 4 bytes long (counting the NUL)
const u32 *tnoms = (u32*)
  "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";

#include "read.h"
#include "sym.h"
#include "two.h"
#include "mem.h"
#include "str.h"
#include "terp.h"

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
    case EOF: case ')': return 0;
    case '(': return read_list(v, i);
    case '"': return read_buffered(v, i, read_str);
    case '\'': return read_quoted(v, i);
    default: return
     ungetc(c, i),
     x = read_buffered(v, i, read_atom),
     y = readz(v, chars(x)),
     nump(y) ? y : intern(v, x); } }

static obj read_list(lips v, FILE *i) {
 obj x, y, c = read_char(i);
 switch (c) {
  case EOF: return 0;
  case ')': return nil;
  default:
   ungetc(c, i);
   bind(x, parse(v, i));
   with(x, y = read_list(v, i));
   bind(y, y);
   return pair(v, x, y); } }

static obj grow_buffer(lips v, obj s) {
  u64 l = b2w(S(s)->len);
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

obj read_file(lips v, FILE *i) {
  obj s = read_buffered(v, i, read_file_loop);
  fclose(i);
  return s; }

obj read_path(lips v, const char *path) {
  FILE *in;
  bind(in, fopen(path, "r"));
  return read_file(v, in); }


u0 write_file(lips v, const char *path, const char *text) {
  FILE *out = fopen(path, "w");
  if (out) {
    for (int c = *text; c; c = *++text) fputc(c, out);
    fclose(out); } }

VM(par_u) {
  RETC(xp = parse(v, stdin),
       v->xp = !xp ? nil : pair(v, xp, nil)); }

VM(slurp) {
  ARY(1);
  xp = *Argv;
  TC(xp, Str);
  RETC(xp = read_path(v, S(xp)->text),
       v->xp = xp ? xp : nil); }

VM(dump) {
  ARY(2);
  TC(Argv[0], Str);
  TC(Argv[1], Str);
  char *p = S(Argv[0])->text,
       *d = S(Argv[1])->text;
  write_file(v, p, d);
  Jump(ret); }

static NoInline obj readz_2(const char *s, i64 rad) {
  static const char *dig = "0123456789abcdef";
  if (!*s) return nil;
  i64 a = 0;
  for (int i, c; (c = *s++); a += i) {
    a *= rad, i = sidx(dig, cmin(c));
    if (i < 0 || rad <= i) return nil; }
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
