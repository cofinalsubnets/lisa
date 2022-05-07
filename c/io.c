#include "em.h"
#include <ctype.h>
// these are the names of the subobjects;
// packing them like this depends on the names
// all being 3 bytes long
const uint32_t *tnoms = (uint32_t*)
  "hom\0num\0two\0vec\0str\0tbl\0sym\0nil";

////
/// " the parser "
//
typedef ob loop(em, FILE*, str, uintptr_t, uintptr_t);
static loop str_loop, atom_loop;
static ob two_in(em, FILE*), read_num(const char*),
          str_in(em, FILE*), read_atom(em, FILE*);

static Inline ob read_buf(em v, FILE* i, loop *loop) {
  str c = cells(v, 2);
  return c ? loop(v, i, c, 0, c->len = 8) : 0; }

static Inline ob str_in(em v, FILE* i) {
  return read_buf(v, i, str_loop); }

static Inline ob read_atom(em v, FILE* i) {
  return read_buf(v, i, atom_loop); }

static int nextc(FILE *i) {
  for (int c;;) switch ((c = getc(i))) {
    case '#': case ';':
      do c = getc(i); while (c != '\n' && c != EOF);
    case ' ': case '\t': case '\n': continue;
    default: return c; } }

ob readq(em v, FILE *i) {
  ob x; return
    !(x = parse(v, i)) ||
    !(x = pair(v, x, nil)) ? 0 :
      pair(v, v->glob[Quote], x); }

ob parse(em v, FILE* i) {
  ob x, y, c = nextc(i);
  switch (c) {
    case EOF: case ')': return 0;
    case '(': return two_in(v, i);
    case '"': return str_in(v, i);
    case '\'': return readq(v, i);
    default: return ungetc(c, i),
      !(x = read_atom(v, i)) ? 0 :
        (y = read_num(getstr(x)->text),
         nump(y) ? y : intern(v, x)); } }

static ob two_in(em v, FILE *i) {
  ob x, y, c = nextc(i);
  return c == ')' ? nil : c == EOF ||
    (ungetc(c, i),
     !(x = parse(v, i)) ||
     !(with(x, y = two_in(v, i)), y)) ? 0 : pair(v, x, y) ; }

static NoInline ob reloop(em v, FILE *i, ob x, uintptr_t n, loop *o) {
  str t; uintptr_t l = b2w(getstr(x)->len); return
    !(with(x, t = cells(v, 2 * l + 1)), t) ? 0 :
      (t->len = 2 * l * sizeof(ob),
       cpyw(t->text, getstr(x)->text, l),
       o(v, i, t, n, 2 * n)); }

static ob atom_loop(em v, FILE *p, str o, uintptr_t n, uintptr_t lim) {
  for (ob x; n < lim;) switch (x = getc(p)) {
    case ' ': case '\n': case '\t': case ';': case '#':
    case '(': case ')': case '\'': case '"': ungetc(x, p);
    case EOF: return o->text[n++] = 0, o->len = n, putstr(o);
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, atom_loop); }

static ob str_loop(em v, FILE *p, str o, uintptr_t n, uintptr_t lim) {
  for (ob x; n < lim;) switch (x = getc(p)) {
    case '\\': if ((x = getc(p)) == EOF)
    case EOF: case '"': return
      o->text[n++] = 0, o->len = n, putstr(o);
    default: o->text[n++] = x; }
  return reloop(v, p, putstr(o), lim, str_loop); }

static Inline int cmin(int c) {
  return c >= 'A' && c <= 'Z' ? c + ('a'-'A') : c; }

static NoInline ob read_num_base(const char *in, int base) {
  static const char *digits = "0123456789abcdef";
  ob out = 0, c = cmin(*in++);
  if (!c) return nil; // fail to parse empty string
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

Ll(par_u) { return
  CallC(xp = parse(v, stdin),
        v->xp = !xp ? nil : pair(v, xp, nil)),
  !xp ? 0 : ApC(ret, xp); }

static void emhomn(em v, ob x, FILE *o) {
  fputc('\\', o);
  if (symp(x)) emit(v, x, o);
  else if (twop(x)) {
    if (symp(A(x))) emit(v, A(x), o);
    emhomn(v, B(x), o); } }

void emit(em v, ob x, FILE *o) {
  switch (Q(x)) {
    default: fputs("()", o); break;
    case Hom: emhomn(v, homnom(v, x), o); break;
    case Num: fprintf(o, "%ld", getnum(x)); break;
    case Str: {
      str s = getstr(x);
      fputc('"', o);
      for (char *t = s->text; *t; fputc(*t++, o))
        if (*t == '"') fputc('\\', o);
      fputc('"', o);
      break; }

    case Tbl: {
      tbl t = gettbl(x);
      fprintf(o, "#tbl:%ld/%ld", t->len, t->cap);
      break; }

    case Sym: {
      sym y = getsym(x);
      (nilp(y->nom) ?
        fprintf(o, "#sym@%lx", (long) y) :
        fputs(getstr(y->nom)->text, o));
      break; }

    case Two: {
      // quotation?
      if (A(x) == v->glob[Quote] &&
          twop(B(x)) &&
          nilp(BB(x)))
        return fputc('\'', o), emit(v, A(B(x)), o);

      for (fputc('(', o);; x = B(x)) {
        emit(v, A(x), o);
        if (!twop(B(x))) break;
        fputc(' ', o); }

      fputc(')', o); } } }

// print to console
Ll(show_u) {
  uintptr_t i, l = getnum(fp->argc);
  if (l) {
    for (i = 0; i < l - 1; i++)
      emit(v, fp->argv[i], stdout),
      fputc(' ', stdout);
    emit(v, xp = fp->argv[i], stdout); }
  return fputc('\n', stdout),
         ApC(ret, xp); }

Ll(putc_u) {
  Arity(1);
  return fputc(getnum(*fp->argv), stdout),
         ApC(ret, xp); }
