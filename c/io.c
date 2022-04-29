#include "lips.h"
#include "vm.h"
#include <ctype.h>
// these are the names of the subobjects
const u32 *tnoms = (u32*)
  "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";
// packing them like this depends on the names
// all being 3 bytes long


////
/// " the parser "
//

typedef FILE *io;
typedef ob read_loop(en, io, str, u64, u64);
static ob read_list(en, io), read_num(const char*),
          read_list(en, io), read_str(en, io),
          read_atom(en, io);
static read_loop read_str_loop, read_atom_loop;

SI ob read_buf(en v, io i, read_loop *loop) {
  str c;
  bind(c, cells(v, 2));
  return loop(v, i, c, 0, c->len = 8); }

SI ob read_str(en v, io i) {
  return read_buf(v, i, read_str_loop); }

SI ob read_atom(en v, io i) {
  return read_buf(v, i, read_atom_loop); }


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


static ob read_list(en v, FILE *i) {
  ob x, y, c = read_char(i);
  switch (c) {
    case EOF: return 0;
    case ')': return nil;
    default:
      ungetc(c, i);
      bind(x, parse(v, i));
      with(x, y = read_list(v, i));
      bind(y, y);
      return pair(v, x, y); } }

SI ob grow_buffer(en v, ob s) {
  u64 l = b2w(S(s)->len);
  str t;
  with(s, t = cells(v, 2*l+1));
  bind(t, t);
  t->len = w2b(2*l);
  cpy64(t->text, S(s)->text, l);
  return _S(t); }

SNI ob reloop(en v, FILE *i, ob x, u64 n, read_loop *loop) {
  bind(x, grow_buffer(v, x));
  return loop(v, i, S(x), n, 2 * n); }

static ob read_atom_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case ' ': case '\n': case '\t': case ';': case '#':
    case '(': case ')': case '\'': case '"':
      ungetc(x, p); case EOF:
      o->text[n++] = 0;
      o->len = n;
      return putstr(o);
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, read_atom_loop); }

static ob read_str_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case '\\': if ((x = getc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0, o->len = n ; return putstr(o); }
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, read_str_loop); }

static ob read_file_loop(en v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case EOF: o->text[n++] = 0, o->len = n; return _S(o);
    default: o->text[n++] = x; }
  return reloop(v, p, _S(o), lim, read_file_loop); }

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
  return _N(out); }

SNI ob read_num(const char *s) {
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
  Jump(ret); }

Vm(slurp) {
  Arity(1);
  xp = *Argv;
  Tc(xp, Str);
  CallC(xp = read_path(v, S(xp)->text),
        v->xp = xp ? xp : nil);
  Jump(ret); }

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

Inline u0 emit(en v, ob x, FILE *o) { writers[Q(x)](v, x, o); }

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
