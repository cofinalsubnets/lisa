#include "la.h"
#include <string.h>
#include <ctype.h>

static str
  rx_atom(la, FILE*, char),
  rx_str(la, FILE*);
static ob
  rx_num(la, str, const char*, int);
static int nextc(FILE*);

////
/// " the parser "
//

// get the next token character from the stream
static int nextc(FILE *i) {
  for (int c;;) switch ((c = fgetc(i))) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (fgetc(i)) {
      case '\n': case EOF: return nextc(i); } } }

static ob rx2(la, FILE*), rx1(la, FILE*);
static Inline ob pull(la v, FILE *i, ob x) {
  return ((ob (*)(la, FILE*, ob))(getnum(*v->sp++)))(v, i, x); }

static ob pret(la v, FILE *i, ob x) { return x; }

static ob pxx(la v, FILE *i, ob x) {
  ob y = *v->sp++;
  return pull(v, i, x ? pair(v, y, x) : x); }

static ob rx2r(la v, FILE *i, ob x) {
  return !x || !Push(putnum(pxx), x) ? pull(v, i, 0) : rx2(v, i); }

static ob pxq(la v, FILE* i, ob x) { return
  x = x ? pair(v, x, nil) : x,
  x = x ? pair(v, v->lex[Quote], x) : x,
  pull(v, i, x); }

static ob rx1(la v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case ')': case EOF: return pull(v, i, 0);
    case '(': return rx2(v, i);
    case '"': return pull(v, i, (ob) rx_str(v, i));
    case '\'': return Push(putnum(pxq)) ? rx1(v, i) : pull(v, i, 0); }
  str a = rx_atom(v, i, c);
  ob x = a ? rx_num(v, a, a->text, 1) : 0;
  return pull(v, i, x); }

static ob rx2(la v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case ')': return pull(v, i, nil);
    case EOF: return pull(v, i, 0);
    default: return
      ungetc(c, i),
      Push(putnum(rx2r)) ? rx1(v, i) : pull(v, i, 0); } }

ob la_rx(la v, FILE *i) { return
  Push(putnum(pret)) ? rx1(v, i) : 0; }

static str mkbuf(la v) {
  str s = cells(v, Width(str) + 1);
  return s ? ini_str(s, sizeof(ob)) : s; }

static str grow_buf(la v, str s) {
  str t;
  size_t len = s->len;
  ob _ = (ob) s;
  with(_, t = cells(v, Width(str) + 2 * b2w(len)));
  s = (str) _;
  if (!t) return 0;
  t = ini_str(t, 2 * len);
  memcpy(t->text, s->text, len);
  return t; }

// read the contents of a string literal into a string
static str rx_str(la v, FILE *p) {
  str o = mkbuf(v);
  for (size_t n = 0, lim = sizeof(ob); o; o = grow_buf(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = fgetc(p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = fgetc(p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin:
        return o->text[n++] = 0, o->len = n, o; }
  return 0; }

// read the characters of an atom (number or symbol)
// into a string
static str rx_atom(la v, FILE *p, char c0) {
  str o = mkbuf(v);
  if (o) o->text[0] = c0;
  for (size_t n = 1, lim = sizeof(ob); o; o = grow_buf(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = fgetc(p)) {
      default: o->text[n++] = x; continue;
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': ungetc(x, p);
      case EOF: return o->text[n++] = 0, o->len = n, o; }
  return 0; }

static NoInline ob rx_numb(la v, str b, const char *in, int sign, int rad) {
  static const char *ds = "0123456789abcdefghijklmnopqrstuvwxyz";
  intptr_t out = 0;
  int c = tolower(*in++);
  if (!c) return (ob) symof(v, b); // fail to parse empty string
  do {
    int d = 0;
    while (ds[d] && ds[d] != c) d++;
    if (d >= rad) return (ob) symof(v, b); // fail to parse oob digit
    out = out * rad + d;
  } while ((c = tolower(*in++)));
  return putnum(sign * out); }

static NoInline ob rx_num(la v, str b, const char *s, int sign) {
  switch (*s) {
    case '+': return rx_num(v, b, s+1, sign);
    case '-': return rx_num(v, b, s+1, -sign);
    case '0': { // with radix // FIXME change this syntax to 10001011{b,s,o,d,z,x,n}
      // numbers can be input in bases 2, 6, 8, 10, 12, 16, 36
      const char *r = "b\2s\6o\10d\12z\14x\20n\44";
      for (char c = tolower(s[1]); *r; r += 2)
        if (*r == c) return rx_numb(v, b, s+2, sign, r[1]); } }
  return rx_numb(v, b, s, sign, 10); }

Vm(rx_u) {
  Have(Width(two));
  sp = setw(sp - Width(two), nil, Width(two));
  Pack();
  ob _ = la_rx(v, stdin);
  Unpack();
  if (!_) return ApC(ret, feof(stdin) ? nil : putnum(1));
  two w = ini_two(hp, _, nil);
  hp += Width(two);
  return ApC(ret, (ob) w); }
