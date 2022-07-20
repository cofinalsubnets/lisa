#include "la.h"
#include "io.h"
#include "chars.h"
#include <ctype.h>

static ob
  buf_atom(pt, FILE*, char),
  rx_num(pt, ob, const char*),
  rx_str(pt, FILE*);
static int nextc(FILE*);

////
/// " the parser "
//

// get the next token character from the stream
static int nextc(FILE *i) {
  for (int c;;) switch ((c = fgetc(i))) {
    default: return c;
    case Space: case Tab: case Newline: continue;
    case NumeralSign: case Semicolon:
      for (;;) switch (fgetc(i)) {
        case Newline: case EndOfFile:
          return nextc(i); } } }

static ob rx2(pt, FILE*), rx_(pt, FILE*);
static Inline ob pull(pt v, FILE *i, ob x) { return
  ((ob (*)(pt, FILE*, ob))(getnum(*v->sp++)))(v, i, x); }

static ob pret(pt v, FILE *i, ob x) { return x; }

static ob pxx(pt v, FILE *i, ob x) { return
  pull(v, i, x ? pair(v, *v->sp++, x) : x); }

static ob rx2r(pt v, FILE *i, ob x) {
  return !x || !Push(putnum(pxx), x) ?
    pull(v, i, 0) : rx2(v, i); }

static ob pxq(pt v, FILE* i, ob x) { return
  x = x ? pair(v, x, nil) : x,
  pull(v, i, x ? pair(v, v->lex[Quote], x) : x); }

static ob rx_(pt v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case RightParen: case EndOfFile: return pull(v, i, 0);
    case LeftParen: return rx2(v, i);
    case DoubleQuote: return pull(v, i, rx_str(v, i));
    case SingleQuote: return Push(putnum(pxq)) ? rx_(v, i) : pull(v, i, 0); }
  ob a = buf_atom(v, i, c);
  return pull(v, i, a ? rx_num(v, a, getstr(a)->text) : 0); }

static ob rx2(pt v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case RightParen: return pull(v, i, nil);
    case EndOfFile: return pull(v, i, 0);
    default: return
      ungetc(c, i),
      Push(putnum(rx2r)) ? rx_(v, i) : pull(v, i, 0); } }

ob rx(pt v, FILE *i) { return
  Push(putnum(pret)) ? rx_(v, i) : 0; }

static str new_buf(pt v) {
  str s = cells(v, Width(str) + 1);
  if (s) s->len = 8, s->ext = 0;
  return s; }

static str grow_buf(pt v, str s) {
  str t; size_t l = b2w(s->len);
  ob _ = putstr(s);
  with(_, t = cells(v, Width(str) + 2 * l));
  s = getstr(_);
  return !t ? 0 :
    (t->len = 2 * l * sizeof(ob),
     t->ext = 0,
     cpyw(t->text, s->text, l),
     t); }

// read the contents of a string literal into a string
static ob rx_str(pt v, FILE *p) {
  str o = new_buf(v);
  for (size_t n = 0, lim = 8; o; o = grow_buf(v, o), lim *= 2)
    for (ob x; n < lim;) switch (x = fgetc(p)) {
      // backslash causes the next character to be read literally
      case Backslash:
        if ((x = fgetc(p)) == EOF)
      case DoubleQuote: case EndOfFile:
          return o->text[n++] = 0, o->len = n, putstr(o);
      default: o->text[n++] = x; }
  return 0; }

// read the characters of an atom into a string
static ob buf_atom(pt v, FILE *p, char ch) {
  str o = new_buf(v);
  if (o) o->text[0] = ch;
  for (size_t n = 1, lim = 8; o; o = grow_buf(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = fgetc(p)) {
      default: o->text[n++] = x; continue;
      // these characters terminate an atom
      case Space: case Newline: case Tab:
      case Semicolon: case NumeralSign:
      case LeftParen: case RightParen:
      case SingleQuote: case DoubleQuote:
        ungetc(x, p);
      case EndOfFile:
        return o->text[n++] = 0, o->len = n, putstr(o); }
  return 0; }

static Inline int cmin(int c) {
  return c >= 'A' && c <= 'Z' ? c + ('a'-'A') : c; }

static NoInline ob rx_numb(pt v, ob b, const char *in, int base) {
  static const char *digits = "0123456789abcdef";
  ob out = 0, c = cmin(*in++);
  if (!c) return intern(v, b); // fail to parse empty string
  do {
    int dig = 0;
    for (const char *ds = digits; *ds && *ds != c; ds++, dig++);
    if (dig >= base) return intern(v, b); // fail to parse oob digit
    out = out * base + dig;
  } while ((c = cmin(*in++)));
  return putZ(out); }

static NoInline ob rx_num(pt v, ob b, const char *s) {
  ob n;
  switch (*s) {
    case Plus: return rx_num(v, b, s+1);
    case Minus: return
      n = rx_num(v, b, s+1),
      !nump(n) ? n : putZ(-getZ(n));
    case NumeralZero: switch (cmin(s[1])) {
      case Radix2: return rx_numb(v, b, s+2, 2);
      case Radix8: return rx_numb(v, b, s+2, 8);
      case Radix10: return rx_numb(v, b, s+2, 10);
      case Radix12: return rx_numb(v, b, s+2, 12);
      case Radix16: return rx_numb(v, b, s+2, 16); } }
  return rx_numb(v, b, s, 10); }

static void emhomn(pt, FILE*, ob);

// s-expression writer
void tx(pt v, FILE *o, ob x) {
  switch (TypeOf(x)) {
    case Hom: return emhomn(v, o, hnom(v, x));
    case Num: return (void) fprintf(o, "%ld", getZ(x));
    case Two:
      for (fputc(LeftParen, o);; x = B(x)) {
        tx(v, o, A(x));
        if (!twop(B(x))) return (void) fputc(RightParen, o);
        fputc(Space, o); }
    case Sym: {
      sym y = getsym(x);
      strp(y->nom) ?
        fputs(getstr(y->nom)->text, o) :
        fprintf(o, "sym@%lx", (long) y);
      return; }
    case Tbl: {
      tbl t = gettbl(x);
      fprintf(o, "#tbl:%ld/%ld", t->len, t->cap);
      return; }
    case Str:
      fputc(DoubleQuote, o);
      for (char *t = getstr(x)->text; *t; fputc(*t++, o))
        if (*t == DoubleQuote) fputc(Backslash, o);
      fputc(DoubleQuote, o);
      return; } }

static void emhomn(pt v, FILE *o, ob x) {
  if (symp(x)) fputc(Backslash, o), tx(v, o, x);
  else if (!twop(x)) fputc(Backslash, o);
  else { // FIXME this is weird
    if (symp(A(x)) || twop(A(x))) emhomn(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) emhomn(v, o, B(x)); } }

#include "vm.h"
Vm(show_u) {
  size_t i, l = getnum(fp->argc);
  if (l > 0) {
    for (i = 0; i < l - 1; i++)
      tx(v, stdout, fp->argv[i]),
      fputc(Space, stdout);
    xp = fp->argv[i];
    tx(v, stdout, xp); }
  return fputc(Newline, stdout),
         ApC(ret, xp); }

Vm(putc_u) { return
  Arity == 0 ? ArityError(1) :
  (fputc(getZ(fp->argv[0]), stdout),
   ApC(ret, xp)); }
