#include "la.h"
#include <ctype.h>
////
/// " the parser "
//
typedef ob loop(ph, fd, str, N, N);
static loop str_loop, atom_loop;
static ob par2(em, fd), par1b(em, ob, const char*),
          par8(em, fd), buf1(em, fd, ch);

static NoInline ob co_loop(ph v, fd i, ob x, N n, loop *o) {
  str t; N l = b2w(getstr(x)->len); return
    !(with(x, t = cells(v, Width(str) + 2 * l)), t) ? 0 :
      (t->len = 2 * l * sizeof(ob),
       t->ext = 0,
       cpyw(t->text, getstr(x)->text, l),
       o(v, i, t, n, 2 * n)); }

static Inline ob par8(ph v, fd i) {
  str c = cells(v, Width(str) + 1);
  return !c ? 0 : (
    c->len = 8,
    c->ext = 0,
    str_loop(v, i, c, 0, 8)); }

static Inline ob buf1(em v, fd i, ch ch) {
  str c = cells(v, Width(str) + 1);
  return !c ? 0 : (
    c->ext = 0,
    c->len = 8,
    c->text[0] = ch,
    atom_loop(v, i, c, 1, 8)); }

static Z nextc(fd i) {
  for (Z c;;) switch ((c = fgetc(i))) {
    default: return c;
    case Space: case Tab: case Newline: continue;
    case NumeralSign: case Semicolon:
      do c = fgetc(i); while (c != '\n' && c != EOF); } }

ob parq(ph v, fd i) {
  ob x; return
    !(x = parse(v, i)) ||
    !(x = pair(v, x, nil)) ? 0 :
      pair(v, v->lex[Quote], x); }

ob parse(ph v, fd i) {
  ob x, c = nextc(i);
  switch (c) {
    case EndOfFile: case RightParen: return 0;
    case LeftParen: return par2(v, i);
    case DoubleQuote: return par8(v, i);
    case SingleQuote: return parq(v, i);
    default:
      return !(x = buf1(v, i, c)) ? 0 :
        par1b(v, x, getstr(x)->text); } }

static ob par2(em v, FILE *i) {
  ob x, y, c = nextc(i);
  return c == RightParen ? nil : c == EndOfFile ||
    (ungetc(c, i),
     !(x = parse(v, i)) ||
     !(with(x, y = par2(v, i)), y)) ? 0 : pair(v, x, y) ; }

static ob atom_loop(em v, FILE *p, str o, uintptr_t n, uintptr_t lim) {
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
  return co_loop(v, p, putstr(o), lim, atom_loop); }

static ob str_loop(em v, FILE *p, str o, uintptr_t n, uintptr_t lim) {
  for (ob x; n < lim;) switch (x = fgetc(p)) {
    // backslash causes the next character to be read literally
    case Backslash:
      if ((x = fgetc(p)) == EOF)
    case EndOfFile: case DoubleQuote:
        return o->text[n++] = 0, o->len = n, putstr(o);
    default:
      o->text[n++] = x; }
  return co_loop(v, p, putstr(o), lim, str_loop); }

static Inline int cmin(int c) {
  return c >= 'A' && c <= 'Z' ? c + ('a'-'A') : c; }

static NoInline ob read_num_base(em v, ob b, const char *in, int base) {
  static const char *digits = "0123456789abcdef";
  ob out = 0, c = cmin(*in++);
  if (!c) return intern(v, b); // fail to parse empty string
  do {
    int digit = 0;
    for (const char *d = digits; *d && *d != c; d++, digit++);
    if (digit >= base) return intern(v, b); // fail to parse oob digit
    out = out * base + digit;
  } while ((c = cmin(*in++)));
  return putnum(out); }

static NoInline ob par1b(em v, ob b, const char *s) {
  ob n;
  switch (*s) {
    case Minus: return nump(n = par1b(v, b, s+1)) ? putnum(-getnum(n)) : n;
    case Plus: return par1b(v, b, s+1);
    case Zero: switch (cmin(s[1])) {
      case Radix2: return read_num_base(v, b, s+2, 2);
      case Radix8: return read_num_base(v, b, s+2, 8);
      case Radix10: return read_num_base(v, b, s+2, 10);
      case Radix12: return read_num_base(v, b, s+2, 12);
      case Radix16: return read_num_base(v, b, s+2, 16); } }
  return read_num_base(v, b, s, 10); }

static void fin_fclose(la v, ob f) {
  fclose(((FILE**)f)[1]); }

Ll(fpar) {
  FILE *in = (FILE*) ip[1].ll;
  return
    Pack(),
    v->xp = parse(v, in),
    Unpack(),
    ApC(ret, xp ? xp : nil); }

Ll(fopen_u) {
  Arity(1);
  xp = fp->argv[0];
  TypeCheck(xp, Str);
  Have(8);
  FILE *in = fopen(getstr(xp)->text, "r");
  if (!in) return ApC(ret, nil);
  ob *k = hp;
  hp += 4;
  k[0] = (ob) fpar;
  k[1] = (ob) in;
  k[2] = 0;
  k[3] = (ob) k;
  two w = (two) hp;
  hp += 4;
  w[0].a = (ob) k, w[0].b = putZ(fin_fclose);
  w[1].a = putW(w), w[1].b = v->fins;
  v->fins = puttwo(w+1);
  return ApC(ret, (ob) k); }
