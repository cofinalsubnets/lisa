#include "la.h"
#include "io.h"
#include <ctype.h>

enum Char {
  LeftParen = '(',
  RightParen = ')',
  SingleQuote = '\'',
  DoubleQuote = '"',
  Backslash = '\\',
  Newline = '\n',
  Space = ' ',
  Tab = '\t',
  NumeralSign = '#',
  Semicolon = ';',
  Plus = '+',
  Minus = '-',
  NumeralZero = '0',
  EndOfFile = EOF, };

static ob
  buf_atom(la, FILE*, char),
  rx_num(la, ob, const char*),
  rx_str(la, FILE*);
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

static ob rx2(la, FILE*), rx_(la, FILE*);
static Inline ob pull(la v, FILE *i, ob x) { return
  ((ob (*)(la, FILE*, ob))(getnum(*v->sp++)))(v, i, x); }

static ob pret(la v, FILE *i, ob x) { return x; }

static ob pxx(la v, FILE *i, ob x) { return
  pull(v, i, x ? pair(v, *v->sp++, x) : x); }

static ob rx2r(la v, FILE *i, ob x) {
  return !x || !Push(putnum(pxx), x) ?
    pull(v, i, 0) : rx2(v, i); }

static ob pxq(la v, FILE* i, ob x) { return
  x = x ? pair(v, x, nil) : x,
  pull(v, i, x ? pair(v, v->lex[Quote], x) : x); }

static ob rx_(la v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case RightParen: case EndOfFile: return pull(v, i, 0);
    case LeftParen: return rx2(v, i);
    case DoubleQuote: return pull(v, i, rx_str(v, i));
    case SingleQuote: return Push(putnum(pxq)) ? rx_(v, i) : pull(v, i, 0); }
  ob a = buf_atom(v, i, c);
  return pull(v, i, a ? rx_num(v, a, getstr(a)->text) : 0); }

static ob rx2(la v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case RightParen: return pull(v, i, nil);
    case EndOfFile: return pull(v, i, 0);
    default: return
      ungetc(c, i),
      Push(putnum(rx2r)) ? rx_(v, i) : pull(v, i, 0); } }

ob rx(la v, FILE *i) { return
  Push(putnum(pret)) ? rx_(v, i) : 0; }

static str new_buf(la v) {
  str s = cells(v, Width(str) + 1);
  if (s) s->len = 8, s->ext = 0;
  return s; }

static str grow_buf(la v, str s) {
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
static ob rx_str(la v, FILE *p) {
  str o = new_buf(v);
  for (size_t n = 0, lim = 8; o; o = grow_buf(v, o), lim *= 2)
    for (ob x; n < lim;) switch (x = fgetc(p)) {
      // backslash causes the next character to be read literally
      case Backslash:
        if ((x = fgetc(p)) != EOF) goto ok;
      case DoubleQuote: case EndOfFile:
        return o->text[n++] = 0, o->len = n, putstr(o);
      default: ok: o->text[n++] = x; }
  return 0; }

// read the characters of an atom into a string
static ob buf_atom(la v, FILE *p, char ch) {
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

static NoInline ob rx_numb(la v, ob b, const char *in, int base) {
  static const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  ob out = 0, c = cmin(*in++);
  if (!c) return intern(v, b); // fail to parse empty string
  do {
    int dig = 0;
    for (const char *ds = digits; *ds && *ds != c; ds++, dig++);
    if (dig >= base) return intern(v, b); // fail to parse oob digit
    out = out * base + dig;
  } while ((c = cmin(*in++)));
  return putZ(out); }

// numbers can be input in bases 2, 6, 8, 10, 12, 16, 36
static char radicize(char c) {
  static const char *radices =
    "b\2s\6o\10d\12z\14x\20n\44";
  for (const char *r = radices; *r; r += 2)
    if (*r == c) return r[1];
  return 0; }

static NoInline ob rx_num(la v, ob b, const char *s) {
  ob n;
  switch (*s) {
    case Plus: return rx_num(v, b, s+1);
    case Minus: return
      n = rx_num(v, b, s+1),
      !nump(n) ? n : putZ(-getZ(n));
    case NumeralZero: {
      char r = radicize(cmin(s[1]));
      if (r) return rx_numb(v, b, s+2, r); } }
  return rx_numb(v, b, s, 10); }

static void emhomn(la, FILE*, ob);

// s-expression writer
void tx(la v, FILE *o, ob x) {
  switch (TypeOf(x)) {
    case Hom: emhomn(v, o, hnom(v, x)); return;
    case Num: fprintf(o, "%ld", getZ(x)); return;
    case Two:
      for (fputc(LeftParen, o);; x = B(x)) {
        tx(v, o, A(x));
        if (!twop(B(x))) {
          fputc(RightParen, o);
          return; }
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

static void emhomn(la v, FILE *o, ob x) {
  if (symp(x)) fputc(Backslash, o), tx(v, o, x);
  else if (!twop(x)) fputc(Backslash, o);
  else { // FIXME this is weird
    if (symp(A(x)) || twop(A(x))) emhomn(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) emhomn(v, o, B(x)); } }

#include <stdarg.h>
static void show_call(la v, mo ip, fr fp) {
  fputc(LeftParen, stderr);
  tx(v, stderr, (ob) ip);
  for (size_t i = 0, argc = getZ(fp->argc); i < argc;)
    fputc(Space, stderr),
    tx(v, stderr, fp->argv[i++]);
  fputc(RightParen, stderr); }

#define bottom (ptr(fp) == v->pool + v->len)
NoInline ob err(la v, ob x, const char *msg, ...) {
  mo ip = v->ip;
  fr fp = v->fp;

  // print error
  fputs("# ", stderr);
  if (!bottom) // show call if possible
    show_call(v, ip, fp),
    fputc(Space, stderr);

  // show message
  va_list xs;
  va_start(xs, msg);
  vfprintf(stderr, msg, xs);
  va_end(xs);
  if (x) fputc(Space, stderr), tx(v, stderr, x);
  fputc(Newline, stderr);

  // show backtrace
  while (!bottom)
    fputs("# à ", stderr),
    show_call(v, ip, fp),
    fputc(Newline, stderr),
    ip = (mo) fp->retp,
    fp = (fr) ((ob*) (fp + 1) + getZ(fp->argc)
                              + getZ(fp->subd));

  // reset and yield
  return
    v->fp = (fr) (v->pool + v->len),
    v->sp = (ob*) v->fp,
    v->xp = nil,
    v->ip = (mo) nil,
    0; }

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
