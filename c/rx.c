#include "la.h"
#include <ctype.h>

#define NumeralSign '#'
#define Semicolon ';'
#define Tab '\t'
#define EndOfFile  EOF

////
/// " the parser "
//
typedef ob loop(pt, FILE*, str, uintptr_t, uintptr_t);
static loop buf_str, buf_atom;
static ob par2(pt, FILE*), rx1_(pt, ob, const char*),
          rx8(pt, FILE*), rx1(pt, FILE*, char);

static NoInline ob co_loop(pt v, FILE *i, ob x, uintptr_t n, loop *o) {
  str t;
  uintptr_t l = b2w(getstr(x)->len);
  with(x, t = cells(v, Width(str) + 2 * l));
  if (!t) return 0;
  t->len = 2 * l * sizeof(ob);
  t->ext = 0;
  cpyw(t->text, getstr(x)->text, l);
  return o(v, i, t, n, 2 * n); }

static ob rx8(pt v, FILE *i) {
  str c = cells(v, Width(str) + 1);
  if (!c) return 0;
  c->len = 8;
  c->ext = 0;
  return buf_str(v, i, c, 0, 8); }

static ob rx1(pt v, FILE *i, char ch) {
  str c = cells(v, Width(str) + 1);
  if (!c) return 0;
  c->ext = 0;
  c->len = 8;
  c->text[0] = ch;
  ob a = buf_atom(v, i, c, 1, 8);
  return a ? rx1_(v, a, getstr(a)->text) : 0; }

static char nextc(FILE *i) {
  for (char c;;) switch ((c = fgetc(i))) {
    default: return c;
    case Space: case Tab: case Newline: continue;
    case NumeralSign: case Semicolon:
      for (;;) switch (fgetc(i)) {
        case Newline: case EndOfFile: return nextc(i); } } }

ob rxq(pt v, FILE *i) {
  ob x; return
    !(x = rx(v, i)) ||
    !(x = pair(v, x, nil)) ? 0 :
      pair(v, v->lex[Quote], x); }

ob rx(pt v, FILE *i) {
  char c = nextc(i);
  switch (c) {
    case EndOfFile: case RightParen: return 0;
    case LeftParen: return par2(v, i);
    case DoubleQuote: return rx8(v, i);
    case SingleQuote: return rxq(v, i);
    default: return rx1(v, i, c); } }

static ob par2(pt v, FILE *i) {
  ob x, y, c = nextc(i);
  switch (c) {
    case RightParen: return nil;
    default:
      ungetc(c, i);
      if ((x = rx(v, i)) &&
          (with(x, y = par2(v, i)), y))
        return pair(v, x, y);
    case EndOfFile: return 0; } }

static ob buf_atom(pt v, FILE *p, str o, uintptr_t n, uintptr_t lim) {
  for (ob x; n < lim;) switch (x = fgetc(p)) {
    // these characters terminate an atom
    case Space: case Newline: case Tab:
    case Semicolon: case NumeralSign:
    case LeftParen: case RightParen:
    case SingleQuote: case DoubleQuote:
      ungetc(x, p);
    case EndOfFile:
      return o->text[n++] = 0, o->len = n, putstr(o);
    default:
      o->text[n++] = x; }
  return co_loop(v, p, putstr(o), lim, buf_atom); }

static ob buf_str(pt v, FILE *p, str o, uintptr_t n, uintptr_t lim) {
  for (ob x; n < lim;) switch (x = fgetc(p)) {
    // backslash causes the next character to be read literally
    case Backslash:
      if ((x = fgetc(p)) == EOF)
    case DoubleQuote: case EndOfFile:
        return o->text[n++] = 0, o->len = n, putstr(o);
    default:
      o->text[n++] = x; }
  return co_loop(v, p, putstr(o), lim, buf_str); }

static Inline int cmin(int c) {
  return c >= 'A' && c <= 'Z' ? c + ('a'-'A') : c; }

static NoInline ob rx1b(pt v, ob b, const char *in, int base) {
  static const char *digits = "0123456789abcdef";
  ob out = 0, c = cmin(*in++);
  if (!c) return intern(v, b); // fail to parse empty string
  do {
    int digit = 0;
    const char *d = digits;
    while (*d && *d != c) d++, digit++;
    if (digit >= base) return intern(v, b); // fail to parse oob digit
    out = out * base + digit;
  } while ((c = cmin(*in++)));
  return putZ(out); }

#define Radix2  'b'
#define Radix8  'o'
#define Radix10 'd'
#define Radix12 'z'
#define Radix16 'x'
#define Plus    '+'
#define Minus   '-'
#define Zero    '0'
static NoInline ob rx1_(pt v, ob b, const char *s) {
  ob n;
  switch (*s) {
    case Minus: return nump(n = rx1_(v, b, s+1)) ? putZ(-getZ(n)) : n;
    case Plus: return rx1_(v, b, s+1);
    case Zero: switch (cmin(s[1])) {
      case Radix2: return rx1b(v, b, s+2, 2);
      case Radix8: return rx1b(v, b, s+2, 8);
      case Radix10: return rx1b(v, b, s+2, 10);
      case Radix12: return rx1b(v, b, s+2, 12);
      case Radix16: return rx1b(v, b, s+2, 16); } }
  return rx1b(v, b, s, 10); }


