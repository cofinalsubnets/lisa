#include "lips.h"
// these are the names of the fundamental types.
// obviously this only works if the type names
// are all 4 bytes long (counting the NUL)
const u32 *tnoms = (u32*)
  "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";

#include "terp.h"
#include <ctype.h>

////
/// " the parser "
//

typedef ob reader(en, FILE*);
typedef ob read_loop(en, FILE*, str, u64, u64);
static reader read_list, read_str, read_atom;
static ob read_list(en, FILE*), read_num(const char*);
static read_loop read_str_loop, read_atom_loop;

static Inline ob read_buffered(en, FILE*, read_loop*);
static NoInline ob grow_buffer(en, ob);

static int read_char(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case '#': case ';':
      do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: return c; } }

ob read_quoted(en v, FILE *i) {
  ob x;
  bind(x, parse(v, i));
  bind(x, pair(v, x, nil));
  return pair(v, Qt, x); }

ob parse(en v, FILE* i) {
  int c = read_char(i);
  ob x, y;
  switch (c) {
    case EOF: case ')': return 0;
    case '(': return read_list(v, i);
    case '"': return read_str(v, i);
    case '\'': return read_quoted(v, i);
    default: ungetc(c, i);
             bind(x, read_atom(v, i));
             y = read_num(S(x)->text);
             return nump(y) ? y : intern(v, x); } }

static Inline ob read_buffered(en v, FILE *i, read_loop *loop) {
  str c;
  bind(c, cells(v, 2));
  return loop(v, i, c, 0, c->len = 8); }

static Inline ob read_str(en v, FILE *i) {
  return read_buffered(v, i, read_str_loop); }

static Inline ob read_atom(en v, FILE *i) {
  return read_buffered(v, i, read_atom_loop); }

static ob read_list(en v, FILE *i) {
  ob x, y, c = read_char(i);
  switch (c) {
    case EOF: return 0;
    case ')': return nil;
    default: ungetc(c, i);
             bind(x, parse(v, i));
             with(x, y = read_list(v, i));
             bind(y, y);
             return pair(v, x, y); } }

static NoInline ob reloop(en v, FILE *i, ob x, u64 n, read_loop *loop) {
  bind(x, grow_buffer(v, x));
  return loop(v, i, S(x), n, 2 * n); }

static NoInline ob grow_buffer(en v, ob s) {
  u64 l = b2w(S(s)->len);
  str t;
  with(s, t = cells(v, 2*l+1));
  bind(t, t);
  t->len = w2b(2*l);
  cpy64(t->text, S(s)->text, l);
  return _S(t); }

static ob read_atom_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case ' ': case '\n': case '\t': case ';': case '#':
    case '(': case ')': case '\'': case '"':
      ungetc(x, p); case EOF:
      o->text[n++] = 0, o->len = n;
      return _S(o);
    default: o->text[n++] = x; }
  return reloop(v, p, _S(o), lim, read_atom_loop); }

static ob read_str_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case '\\': if ((x = getc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0, o->len = n ;return _S(o); }
    default: o->text[n++] = x; }
  return reloop(v, p, _S(o), lim, read_str_loop); }

static ob read_file_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case EOF: o->text[n++] = 0, o->len = n; return _S(o);
    default: o->text[n++] = x; }
  return reloop(v, p, _S(o), lim, read_file_loop); }

static NoInline ob read_num_base(const char *in, int base) {
  static const char *digits = "0123456789abcdef";
  int c = tolower(*in++);
  if (!c) return nil; // fail to parse empty string
  i64 out = 0;
  do {
    int digit = 0;
    for (const char *d = digits; *d && *d != c; d++, digit++);
    if (digit >= base) return nil; // fail to parse oob digit
    out = out * base + digit;
  } while ((c = tolower(*in++)));
  return _N(out); }

static NoInline ob read_num(const char *s) {
  ob n;
  switch (*s) {
    case '-': return nump(n = read_num(s+1)) ? _N(-N(n)) : n;
    case '+': return read_num(s+1);
    case '0': switch (tolower(s[1])) {
      case 'b': return read_num_base(s+2, 2);
      case 'o': return read_num_base(s+2, 8);
      case 'd': return read_num_base(s+2, 10);
      case 'z': return read_num_base(s+2, 12);
      case 'x': return read_num_base(s+2, 16); } }
  return read_num_base(s, 10); }


ob read_file(en v, FILE *i) {
  ob s = read_buffered(v, i, read_file_loop);
  fclose(i);
  return s; }

ob read_path(en v, const char *path) {
  FILE *in;
  bind(in, fopen(path, "r"));
  return read_file(v, in); }

Vm(par_u) {
  CallC(xp = parse(v, stdin),
        // collapses two kinds of failure into one
        v->xp = !xp ? nil : pair(v, xp, nil));
  bind(xp, xp);
  Jump(ret); }

Vm(slurp) {
  Arity(1);
  xp = *Argv;
  Tc(xp, Str);
  CallC(xp = read_path(v, S(xp)->text),
        v->xp = xp ? xp : nil);
  Jump(ret); }
