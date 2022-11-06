#include "la.h"
#include <string.h>
#include <ctype.h>

Vm(rxc_f) { return ApC(ret, putnum(fgetc(stdin))); }

static str
  rxatomstr(la, FILE*),
  rxstr(la, FILE*);
static ob
  rxatom(la, str),
  rxret(la, FILE*, ob),
  rx(la, FILE*),
  rxtwo(la, FILE*);

ob la_rx(la v, FILE *i) { return
  pushs(v, rxret, NULL) ? rx(v, i) : 0; }

////
/// " the parser "
//

// get the next token character from the stream
static int rxchar(FILE *i) {
  for (int c;;) switch ((c = fgetc(i))) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (fgetc(i)) {
      case '\n': case EOF: return rxchar(i); } } }

static Inline ob pull(la v, FILE *i, ob x) { return
  ((ob (*)(la, FILE*, ob))(*v->sp++))(v, i, x); }

static ob rxret(la v, FILE *i, ob x) { return x; }

static ob rxtwo_cons(la v, FILE *i, ob x) {
  ob y = *v->sp++;
  return pull(v, i, x ? (ob) pair(v, y, x) : x); }

static ob rxtwo_cont(la v, FILE *i, ob x) {
  return !x || !pushs(v, rxtwo_cons, x, NULL) ?
    pull(v, i, 0) :
    rxtwo(v, i); }

static ob rxq(la v, FILE* i, ob x) { return
  x = x ? (ob) pair(v, x, nil) : x,
  x = x ? (ob) pair(v, (ob) v->lex[Quote], x) : x,
  pull(v, i, x); }

static NoInline ob rx(la v, FILE *i) {
  int c = rxchar(i);
  switch (c) {
    case ')': case EOF: return pull(v, i, 0);
    case '(': return rxtwo(v, i);
    case '"': return pull(v, i, (ob) rxstr(v, i));
    case '\'': return
      pushs(v, rxq, NULL) ? rx(v, i) : pull(v, i, 0); }
  ungetc(c, i);
  str a = rxatomstr(v, i);
  ob x = a ? rxatom(v, a) : 0;
  return pull(v, i, x); }

static NoInline ob rxtwo(la v, FILE *i) {
  int c = rxchar(i);
  switch (c) {
    case ')': return pull(v, i, nil);
    case EOF: return pull(v, i, 0);
    default: return
      ungetc(c, i),
      pushs(v, rxtwo_cont, NULL) ?
        rx(v, i) :
        pull(v, i, 0); } }

static str mkbuf(la v) {
  str s = cells(v, Width(str) + 1);
  return s ? ini_str(s, sizeof(ob)) : s; }

static str buf_grow(la v, str s) {
  str t;
  size_t len = s->len;
  with(s, t = cells(v, Width(str) + 2 * b2w(len)));
  if (t)
    ini_str(t, 2 * len),
    memcpy(t->text, s->text, len);
  return t; }

// read the contents of a string literal into a string
static str rxstr(la v, FILE *p) {
  str o = mkbuf(v);
  for (size_t n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = fgetc(p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = fgetc(p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }

// read the characters of an atom (number or symbol)
// into a string
static str rxatomstr(la v, FILE *p) {
  str o = mkbuf(v);
  for (size_t n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = fgetc(p)) {
      default: o->text[n++] = x; continue;
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': ungetc(x, p);
      case EOF: return o->len = n, o; }
  return 0; }

static NoInline ob rxatom_n(la v, str b, size_t inset, int sign, int rad) {
  static const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  size_t len = b->len;
  if (inset >= len) fail: return (ob) symof(v, b);
  intptr_t out = 0;
  do {
    int dig = 0, c = tolower(b->text[inset++]);
    while (digits[dig] && digits[dig] != c) dig++;
    if (dig >= rad) goto fail;
    out = out * rad + dig;
  } while (inset < len);
  return putnum(sign * out); }

static NoInline ob rxatom(la v, str b) {
  size_t i = 0;
  int sign = 1;
loop:
  switch (b->text[i]) {
    case '+': i += 1; goto loop;
    case '-': i += 1, sign *= -1; goto loop;
    case '0': { // with radix
      // numbers can be input in bases 2, 6, 8, 10, 12, 16, 36
      const char *r = "b\2s\6o\10d\12z\14x\20n\44";
      for (char c = tolower(b->text[i+1]); *r; r += 2)
        if (*r == c) return rxatom_n(v, b, i+2, sign, r[1]); } }
  return rxatom_n(v, b, i, sign, 10); }
