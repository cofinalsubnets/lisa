#include "la.h"
#include "vm.h"

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
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (fgetc(i)) {
      case '\n': case EOF: return nextc(i); } } }

static ob rx2(la, FILE*), rx_(la, FILE*);
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

static ob rx_(la v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case ')': case EOF: return pull(v, i, 0);
    case '(': return rx2(v, i);
    case '"': return pull(v, i, rx_str(v, i));
    case '\'': return Push(putnum(pxq)) ? rx_(v, i) : pull(v, i, 0); }
  ob a = buf_atom(v, i, c);
  a = a ? rx_num(v, a, ((str)a)->text) : a;
  return pull(v, i, a); }

static ob rx2(la v, FILE *i) {
  int c = nextc(i);
  switch (c) {
    case ')': return pull(v, i, nil);
    case EOF: return pull(v, i, 0);
    default: return
      ungetc(c, i),
      Push(putnum(rx2r)) ? rx_(v, i) : pull(v, i, 0); } }

ob la_rx_f(la v, FILE *i) { return
  Push(putnum(pret)) ? rx_(v, i) : 0; }

ob la_rx_s(la v, const char **s) {
  FILE *i = fmemopen((char*) *s, slen(*s), "r");
  if (!i) return 0;
  ob _ = la_rx_f(v, i);
  if (_) *s += ftell(i);
  fclose(i);
  return _; }

static str new_buf(la v) {
  str s = cells(v, Width(str) + 1);
  if (s) s->len = 8, s->disp = disp, s->mtbl = mtbl_str;
  return s; }

static str grow_buf(la v, str s) {
  str t; size_t l = b2w(s->len);
  ob _ = (ob) s;
  with(_, t = cells(v, Width(str) + 2 * l));
  s = (str) _;
  if (!t) return 0;
  t->len = 2 * l * sizeof(ob);
  t->disp = disp;
  t->mtbl = mtbl_str;
  cpyw(t->text, s->text, l);
  return t; }

// read the contents of a string literal into a string
static ob rx_str(la v, FILE *p) {
  str o = new_buf(v);
  for (size_t n = 0, lim = 8; o; o = grow_buf(v, o), lim *= 2)
    for (ob x; n < lim;) switch (x = fgetc(p)) {
      // backslash causes the next character to be read literally
      case '\\': if ((x = fgetc(p)) != EOF) goto ok;
      case '"': case EOF: return o->text[n++] = 0, o->len = n, (ob) o;
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
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"':
        ungetc(x, p);
      case EOF: return
        o->text[n++] = 0,
        o->len = n,
        (ob) o; }
  return 0; }

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
  return putnum(out); }

// numbers can be input in bases 2, 6, 8, 10, 12, 16, 36
static char radicize(char c) {
  static const char *radices = "b\2s\6o\10d\12z\14x\20n\44";
  for (const char *r = radices; *r; r += 2) if (*r == c) return r[1];
  return 0; }

static NoInline ob rx_num(la v, ob b, const char *s) {
  ob n;
  switch (*s) {
    case '+': return rx_num(v, b, s+1);
    case '-': return
      n = rx_num(v, b, s+1),
      !nump(n) ? n : putnum(-getnum(n));
    case '0': {
      char r = radicize(cmin(s[1]));
      if (r) return rx_numb(v, b, s+2, r); } }
  return rx_numb(v, b, s, 10); }
