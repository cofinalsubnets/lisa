#include "em.h"
#include <ctype.h>
// these are the names of the subobjects;
// packing them like this depends on the names
// all being 3 bytes long
const u32 *tnoms = (u32*)
  "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";

////
/// " the parser "
//
typedef ob read_loop(em, FILE*, str, u64, u64);
static ob two_in(em, FILE*), read_num(const char*),
          str_in(em, FILE*), read_atom(em, FILE*);
static read_loop str_loop, atom_loop;

static Inline ob read_buf(em v, FILE* i, read_loop *loop) {
  str c;
  bind(c, cells(v, 2));
  return loop(v, i, c, 0, c->len = 8); }

static Inline ob str_in(em v, FILE* i) {
  return read_buf(v, i, str_loop); }

static Inline ob read_atom(em v, FILE* i) {
  return read_buf(v, i, atom_loop); }

static int read_char(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case '#': case ';':
      do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: return c; } }

ob read_quoted(em v, FILE *i) {
  ob x;
  bind(x, parse(v, i));
  bind(x, pair(v, x, nil));
  return pair(v, Qt, x); }

ob parse(em v, FILE* i) {
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

static ob two_in(em v, FILE *i) {
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

static Inline ob grow_buffer(em v, ob s) {
  u64 l = b2w(getstr(s)->len);
  str t;
  with(s, t = cells(v, 2 * l + 1));
  bind(t, t);
  t->len = 2 * l * sizeof(ob);
  cpy64(t->text, getstr(s)->text, l);
  return putstr(t); }

static NoInline ob reloop(em v, FILE *i, ob x, u64 n, read_loop *loop) {
  bind(x, grow_buffer(v, x));
  return loop(v, i, getstr(x), n, 2 * n); }

static ob atom_loop(em v, FILE *p, str o, u64 n, u64 lim) {
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

static ob str_loop(em v, FILE *p, str o, u64 n, u64 lim) {
  ob x;
  while (n < lim) switch (x = getc(p)) {
    case '\\': if ((x = getc(p)) == EOF) {
    case EOF: case '"': o->text[n++] = 0, o->len = n ; return putstr(o); }
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, str_loop); }

static NoInline ob read_num_base(const char *in, int base) {
  static const char *digits = "0123456789abcdef";
  int c = cmin(*in++);
  if (!c) return nil; // fail to parse empty string
  i64 out = 0;
  do {
    int digit = 0;
    for (const char *d = digits; *d && *d != c; d++, digit++);
    if (digit >= base) return nil; // fail to parse oob digit
    out = out * base + digit;
  } while ((c = cmin(*in++)));
  return putnum(out); }

static NoInline ob read_num(const char *s) {
  ob n;
  switch (*s) {
    case '-': return nump(n = read_num(s+1)) ? putnum(-getnum(n)) : n;
    case '+': return read_num(s+1);
    case '0': switch (cmin(s[1])) {
      case 'b': return read_num_base(s+2, 2);
      case 'o': return read_num_base(s+2, 8);
      case 'd': return read_num_base(s+2, 10);
      case 'z': return read_num_base(s+2, 12);
      case 'x': return read_num_base(s+2, 16); } }
  return read_num_base(s, 10); }

Vm(par_u) {
  CallC(xp = parse(v, stdin),
        v->xp = !xp ? nil : pair(v, xp, nil));
  bind(xp, xp);
  return ApC(ret, xp); }

static void emhomn(em v, ob x, FILE *o) {
  fputc('\\', o);
  switch (Q(x)) { case Sym: return emit(v, x, o);
                  case Two: if (symp(A(x))) emit(v, A(x), o);
                            emhomn(v, B(x), o);
                  default: } }

void emit(em v, ob x, FILE *o) {
  enum class q = Q(x);
  switch (q) {
    default: return (void) fputs("()", o);
    case Hom: return emhomn(v, homnom(v, x), o);
    case Num: return (void) fprintf(o, "%ld", getnum(x));

    case Str: {
      str s = getstr(x);

      fputc('"', o);
      for (char *t = s->text; *t; fputc(*t++, o))
        if (*t == '"') fputc('\\', o);

      return (void) fputc('"', o); }

    case Tbl: {
      tbl t = gettbl(x);
      return (void) fprintf(o, "#tbl:%ld/%ld", t->len, t->cap); }

    case Sym: {
      sym y = getsym(x);
      return (void) (nilp(y->nom) ?
        fprintf(o, "#sym@%lx", (long) y) :
        fputs(getstr(y->nom)->text, o)); }

    case Two: {
      bool is_quotation =
        A(x) == Qt &&
        twop(B(x)) &&
        nilp(BB(x));

      if (is_quotation)
        return fputc('\'', o),
               emit(v, A(B(x)), o);

      for (fputc('(', o);; x = B(x)) {
        emit(v, A(x), o);
        if (!twop(B(x))) break;
        fputc(' ', o); }

      return (void) fputc(')', o); } } }

// print to console
Ll(show_u) {
  u64 l = getnum(Argc), i;
  if (l) { for (i = 0; i < l - 1; i++)
             emsep(v, Argv[i], stdout, ' ');
           emit(v, xp = Argv[i], stdout); }
  fputc('\n', stdout);
  return ApC(ret, xp); }

Ll(putc_u) {
  Arity(1);
  fputc(getnum(*Argv), stdout);
  return ApC(ret, xp); }
