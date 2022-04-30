#include "lips.h"
#include "ll.h"
#include <ctype.h>
// these are the names of the subobjects;
// packing them like this depends on the names
// all being 3 bytes long
const u32 *tnoms = (u32*)
  "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";


////
/// " the parser "
//

typedef FILE *io;
typedef ob read_loop(en, io, str, u64, u64);
static ob two_in(en, io), read_num(const char*),
          str_in(en, io), read_atom(en, io);
static read_loop str_loop, atom_loop;

SI ob read_buf(en v, io i, read_loop *loop) {
  str c;
  bind(c, cells(v, 2));
  return loop(v, i, c, 0, c->len = 8); }

SI ob str_in(en v, io i) {
  return read_buf(v, i, str_loop); }

SI ob read_atom(en v, io i) {
  return read_buf(v, i, atom_loop); }

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
    case '(': return two_in(v, i);
    case '"': return str_in(v, i);
    case '\'': return read_quoted(v, i);
    default: ungetc(c, i);
             bind(x, read_atom(v, i));
             y = read_num(getstr(x)->text);
             return nump(y) ? y : intern(v, x); } }

static ob two_in(en v, FILE *i) {
  ob x, y, c = read_char(i);
  switch (c) {
    case EOF: return 0;
    case ')': return nil;
    default:
      ungetc(c, i);
      bind(x, parse(v, i));
      with(x, y = two_in(v, i));
      bind(y, y);
      return pair(v, x, y); } }

SI ob grow_buffer(en v, ob s) {
  u64 l = b2w(getstr(s)->len);
  str t;
  with(s, t = cells(v, 2 * l + 1));
  bind(t, t);
  t->len = 2 * l * sizeof(ob);
  cpy64(t->text, getstr(s)->text, l);
  return putstr(t); }

SNI ob reloop(en v, FILE *i, ob x, u64 n, read_loop *loop) {
  bind(x, grow_buffer(v, x));
  return loop(v, i, getstr(x), n, 2 * n); }

static ob atom_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case ' ': case '\n': case '\t': case ';': case '#':
    case '(': case ')': case '\'': case '"':
      ungetc(x, p); case EOF:
      o->text[n++] = 0;
      o->len = n;
      return putstr(o);
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, atom_loop); }

static ob str_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case '\\': if ((x = getc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0, o->len = n ; return putstr(o); }
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, str_loop); }

static ob read_file_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case EOF: o->text[n++] = 0, o->len = n; return putstr(o);
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, read_file_loop); }

SNI ob read_num_base(const char *in, int base) {
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
  return putnum(out); }

SNI ob read_num(const char *s) {
  ob n;
  switch (*s) {
    case '-': return nump(n = read_num(s+1)) ? putnum(-getnum(n)) : n;
    case '+': return read_num(s+1);
    case '0': switch (tolower(s[1])) {
      case 'b': return read_num_base(s+2, 2);
      case 'o': return read_num_base(s+2, 8);
      case 'd': return read_num_base(s+2, 10);
      case 'z': return read_num_base(s+2, 12);
      case 'x': return read_num_base(s+2, 16); } }
  return read_num_base(s, 10); }


ob read_file(en v, FILE *i) {
  ob s = read_buf(v, i, read_file_loop);
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
  return ApC(ret, xp); }

Vm(slurp) {
  Arity(1);
  xp = *Argv;
  Tc(xp, Str);
  CallC(xp = read_path(v, getstr(xp)->text),
        v->xp = xp ? xp : nil);
  return ApC(ret, xp); }

typedef u0 writer(en, ob, FILE*);
static writer nil_out, two_out, num_out, vec_out,
              str_out, sym_out, tbl_out, hom_out;
static writer *writers[] = {
  [Hom] = hom_out, [Num] = num_out, [Tbl] = tbl_out, [Nil] = nil_out,
  [Str] = str_out, [Vec] = vec_out, [Sym] = sym_out, [Two] = two_out, };

static u0 nil_out(en v, ob x, FILE *o) { fputs("()", o); }
static u0 num_out(en v, ob x, FILE *o) {
  fprintf(o, "%ld", getnum(x)); }
static u0 sym_out(en v, ob x, FILE *o) {
  sym y = getsym(x);
  nilp(y->nom) ? fprintf(o, "#sym@%lx", (long) y) :
                 fputs(getstr(y->nom)->text, o); }

static u0 vec_out(en v, ob x, FILE *o) {
  vec e = getvec(x);
  fprintf(o, "#vec:%ld", (long) e->len); }

static u0 emhomn(en v, ob x, FILE *o) {
  fputc('\\', o);
  switch (Q(x)) {
    case Sym: return sym_out(v, x, o);
    case Two: if (symp(A(x))) sym_out(v, A(x), o);
              emhomn(v, B(x), o);
    default: } }

static u0 hom_out(en v, ob x, FILE *o) {
  emhomn(v, homnom(v, x), o); }

static u0 tbl_out(en v, ob x, FILE *o) {
  tbl t = gettbl(x);
  fprintf(o, "#tbl:%ld/%ld", (long)t->len, (long)t->cap); }

static u0 str_out(en v, ob x, FILE *o) {
  str s = getstr(x);
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

Inline u0 emit(en v, ob x, FILE *o) { writers[Q(x)](v, x, o); }

// print to console
Vm(em_u) {
  u64 l = getnum(Argc), i;
  if (l) {
    for (i = 0; i < l - 1; i++)
      emsep(v, Argv[i], stdout, ' ');
    emit(v, xp = Argv[i], stdout); }
  fputc('\n', stdout);
  return ApC(ret, xp); }

Vm(putc_u) {
  Arity(1);
  fputc(getnum(*Argv), stdout);
  return ApC(ret, xp); }

Vm(dump) {
  Arity(2);
  TypeCheck(Argv[0], Str);
  TypeCheck(Argv[1], Str);
  char *p = getstr(Argv[0])->text,
       *d = getstr(Argv[1])->text;
  write_file(v, p, d); // FIXME handle failure
  return ApC(ret, xp); }

u1 write_file(en v, const char *path, const char *text) {
  FILE *out;
  bind(out, fopen(path, "w"));
  u1 r = true;
  for (char c = *text; r && c; c = *++text)
    r = fputc(c, out) != EOF;
  fclose(out);
  return r; }
