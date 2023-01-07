#include "i.h"
// in
#include <ctype.h>

static str rx_atom_str(la, FILE*), rx_str(la, FILE*);
static ob rx_atom(la, str), rxr(la, FILE*), rx_two(la, FILE*);

static ob rx_ret(la v, FILE* i, ob x) { return x; }

enum status receives(la v, FILE *i) {
  enum status s = receive(v, i);
  if (s == Eof) return nil;
  ob x = v->xp;
  with(x, s = receives(v, i));
  if (s != Ok) return s;
  x = (ob) pair(v, x, v->xp);
  if (!x) return OomError;
  v->xp = x;
  return Ok; }

// should distinguish between OOM and parse error
enum status receive(la v, FILE* i) {
  ob x = pushs(v, rx_ret, NULL) ? rxr(v, i) : 0;
  return x ? (v->xp = x, Ok) : feof(i) ? Eof : SyntaxError; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

// get the next token character from the stream
static NoInline int rx_char(FILE* i) {
  for (int c;;) switch ((c = getc(i))) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (getc(i)) {
      case '\n': case EOF: return rx_char(i); } } }

static Inline ob rx_pull(la v, FILE *i, ob x) { return
  ((ob (*)(la, FILE*, ob))(*v->sp++))(v, i, x); }

static ob rx_two_cons(la v, FILE* i, ob x) {
  ob y = *v->sp++; return
    rx_pull(v, i, x ? (ob) pair(v, y, x) : x); }

static ob rx_two_cont(la v, FILE* i, ob x) {
  return !x || !pushs(v, rx_two_cons, x, NULL) ?
    rx_pull(v, i, 0) : rx_two(v, i); }

static ob rx_q(la v, FILE* i, ob x) { return
  x = x ? (ob) pair(v, x, nil) : x,
  x = x ? (ob) pair(v, (ob) v->lex.quote, x) : x,
  rx_pull(v, i, x); }

static NoInline ob rxr(la v, FILE* i) {
  int c = rx_char(i);
  switch (c) {
    case ')': case EOF: return rx_pull(v, i, 0);
    case '(': return rx_two(v, i);
    case '"': return rx_pull(v, i, (ob) rx_str(v, i));
    case '\'': return
      pushs(v, rx_q, NULL) ? rxr(v, i) : rx_pull(v, i, 0); }
  ungetc(c, i);
  str a = rx_atom_str(v, i);
  ob x = a ? rx_atom(v, a) : 0;
  return rx_pull(v, i, x); }

static NoInline ob rx_two(la v, FILE* i) {
  int c = rx_char(i);
  switch (c) {
    case ')': return rx_pull(v, i, nil);
    case EOF: return rx_pull(v, i, 0);
    default: return
      ungetc(c, i),
      pushs(v, rx_two_cont, NULL) ?
        rxr(v, i) : rx_pull(v, i, 0); } }

static str mkbuf(la v) {
  str s = cells(v, Width(struct str) + 1);
  return s ? str_ini(s, sizeof(ob)) : s; }

static str buf_grow(la v, str s) {
  str t; size_t len = s->len;
  with(s, t = cells(v, Width(struct str) + 2 * b2w(len)));
  if (!t) return t;
  return memcpy(t->text, s->text, len),
         str_ini(t, 2 * len); }

// read the contents of a string literal into a string
static str rx_str(la v, FILE* p) {
  str o = mkbuf(v);
  for (U n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = getc(p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }

// read the characters of an atom (number or symbol)
// into a string
static str rx_atom_str(la v, FILE* p) {
  str o = mkbuf(v);
  for (U n = 0, lim = sizeof(ob); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      default: o->text[n++] = x; continue;
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': ungetc(x, p);
      case EOF: return o->len = n, o; }
  return 0; }

static NoInline ob rx_atom_n(la v, str b, U inset, int sign, int rad) {
  static const char *digits = "0123456789abcdefghijklmnopqrstuvwxyz";
  U len = b->len;
  if (inset >= len) fail: return (ob) symof(v, b);
  I out = 0;
  do {
    int dig = 0, c = tolower(b->text[inset++]);
    while (digits[dig] && digits[dig] != c) dig++;
    if (dig >= rad) goto fail;
    out = out * rad + dig;
  } while (inset < len);
  return putnum(sign * out); }

static NoInline ob rx_atom(la v, str b) {
  size_t i = 0, len = b->len;
  int sign = 1;
  while (i < len) switch (b->text[i]) {
    case '+': i += 1; continue;
    case '-': i += 1, sign *= -1; continue;
    case '0': if (i+1 < len) {
      const char *r = "b\2s\6o\10d\12z\14x\20n\44";
      for (char c = tolower(b->text[i+1]); *r; r += 2)
        if (*r == c) return rx_atom_n(v, b, i+2, sign, r[1]); }
    default: goto out; } out:
  return rx_atom_n(v, b, i, sign, 10); }

// out

static void tx_nom(la, FILE*, ob);
void transmit(la v, FILE* o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == act) ((typ) GF(x))->emit(v, o, x);
  else tx_nom(v, o, hnom(v, (mo) x)); }

// print a function name // this is weird
static NoInline void tx_nom(la v, FILE* o, ob x) {
  if (symp(x)) putc('\\', o), transmit(v, o, x);
  else if (!twop(x)) putc('\\', o);
  else {
    if (symp(A(x)) || twop(A(x))) tx_nom(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) tx_nom(v, o, B(x)); } }

#include "vm.h"

Vm(txc_f) { return !fp->argc ?
  Yield(ArityError, putnum(1)) :
  ApC(ret, putnum(putc(getnum(fp->argv[0]), stdout))); }

Vm(tx_f) {
  U i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      transmit(v, stdout, fp->argv[i++]),
      putc(' ', stdout);
    xp = fp->argv[i];
    transmit(v, stdout, xp); }
  return putc('\n', stdout), ApC(ret, xp); }

Vm(rxc_f) { return ApC(ret, putnum(getc(stdin))); }


