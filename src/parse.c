#include "i.h"

// internal parser functions
static string
  rx_lit_str(state, FILE*);
static word
  rx_ret(state, FILE*, word),
  rxr(state, FILE*),
  rx2(state, FILE*),
  rx_a(state, FILE*);

static Inline word pull(state v, FILE *i, word x) { return
  ((word (*)(state, FILE*, word)) pop1(v))(v, i, x); }

// FIXME should distinguish between OOM and parse error
enum status rx_file(state v, FILE *i) {
  word x; return
    !push1(v, (word) rx_ret) ? Oom :
    !(x = rxr(v, i)) ? feof(i) ? Eof : Dom :
    push1(v, x) ? Ok : Oom; }

enum status NoInline receive2(state f, const char *_i) {
  size_t len = strlen(_i);
  char *i = malloc(len + 1);
  if (!i) return Oom;
  memcpy(i, _i, len);
  i[len] = 0;
  FILE *in = fmemopen(i, len, "r");
  if (!in) return free(i), Oom;
  enum status s = rx_file(f, in);
  return fclose(in), free(i), s; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

// get the next token character from the stream
static NoInline int rx_char(FILE *i) {
  for (int c;;) switch (c = getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (getc(i)) {
      case '\n': case EOF: return rx_char(i); } } }

static word rx_ret(state v, FILE* i, word x) { return x; }

static word rx2x(state v, FILE* i, word x) {
  word y = pop1(v);
  return pull(v, i, x ? (word) cons(v, y, x) : x); }

static word rx2k(state v, FILE* i, word x) { return
  !x || !push2(v, (word) rx2x, x) ?
    pull(v, i, 0) : rx2(v, i); }

static word rx_q_cont(state f, FILE *i, word x) {
  x = x ? (word) cons(f, x, nil) : x;
  return pull(f, i, x); }

static NoInline word rxr(state l, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(l, i, 0);
    case '(': return rx2(l, i);
    case '"': return pull(l, i, (word) rx_lit_str(l, i));
    case '\'': return push1(l, (word) rx_q_cont) ? rxr(l, i) : pull(l, i, 0);
    default: return ungetc(c, i), rx_a(l, i); } }

static word rx2(state l, FILE* i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(l, i, nil);
    default: return ungetc(c, i),
      push1(l, (word) rx2k) ? rxr(l, i) : pull(l, i, 0); } }

static string buf_new(state f) {
  string s = cells(f, Width(struct string) + 1);
  if (s) s->ap = data, s->typ = String, s->len = sizeof(word);
  return s; }

static NoInline string buf_grow(state f, string s) {
  string t; size_t len = s->len;
  avec(f, s, t = cells(f, Width(struct string) + 2 * b2w(len)));
  if (t) t->ap = data, t->typ = String, t->len = 2 * len,
         memcpy(t->text, s->text, len);
  return t; }

// read the contents of a string literal into a string
static NoInline string rx_lit_str(state v, FILE* p) {
  string o = buf_new(v);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(v, o), lim *= 2)
    for (int x; n < lim;) switch (x = getc(p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = getc(p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }

static NoInline word rx_a(state l, FILE *in) {
  string a = buf_new(l);
  for (size_t n = 0, lim = sizeof(word); a; a = buf_grow(l, a), lim *= 2)
    for (int x; n < lim;) switch (x = getc(in)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': ungetc(x, in);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = x; continue; } out:
  if (!a) return pull(l, in, 0);
  char *e; long n = strtol(a->text, &e, 0);
  return pull(l, in, *e == 0 ? putnum(n) : (word) a); }

typedef struct parsing {
  string in;
  word out, cur;
} P;

static P *p_cstr(state l, const char *in) {
  string s = strof(l, in);
  if (!s || !push1(l, (word) s)) return 0;
  P *p = (P*) mo_n(l, Width(P));
  if (p) p->in = (string) pop1(l), p->cur = nil;
  return p; }

static int p_getc(P *p) {
  size_t i = getnum(p->cur);
  if (i == p->in->len) return EOF;
  p->cur += 2;
  return p->in->text[i]; }
static void p_ungetc(P *p, int _) { p->cur -= 2; }

static int p_rxc(P *p) {
  for (int c;;) switch (c = p_getc(p)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (p_getc(p)) {
      case '\n': case EOF: return p_rxc(p); } } }

static NoInline string p_rx_lit_str(state l, P **p) {
  string o = buf_new(l);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(l, o), lim *= 2)
    for (int x; n < lim;) switch (x = p_getc(*p)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = p_getc(*p)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }

static Inline word p_pull(state l, P **p, word x) {
  word (*f)(state, P**, word) = (void*) pop1(l);
  return f(l, p, x); }

static word p_rx_ret(state l, P **p, word x) {
  return x; }

static word p_rx2x(state l, P **p, word x) {
  word y = pop1(l),
       z = x ? (word) cons(l, y, x) : x;
  return p_pull(l, p, z); }

static string
  p_rx_lit_str(state, P**);
static word
  p_rx_q_cont(state, P**, word),
  p_rx_a(state, P**),
  p_rx2(state, P**),
  p_rxr(state, P**);

static word p_rx_a(state l, P **p) {
  string a = buf_new(l);
  for (size_t n = 0, lim = sizeof(word); a; a = buf_grow(l, a), lim *= 2)
    for (int x; n < lim;) switch (x = p_getc(*p)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': p_ungetc(*p, x);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = x; continue; } out:
  if (!a) return p_pull(l, p, 0);
  char *e; long n = strtol(a->text, &e, 0);
  return p_pull(l, p, *e == 0 ? putnum(n) : (word) a); }

static word p_rx_q_cont(state l, P **p, word x) {
  x = x ? (word) cons(l, x, nil) : x;
  return p_pull(l, p, x); }

static word p_rx2k(state l, P **p, word x) { return
  !x || !push2(l, (word) p_rx2x, x) ?
    p_pull(l, p, 0) : p_rx2(l, p); }

static word p_rx2(state l, P **p) {
  int c = p_rxc(*p); switch (c) {
    case ')': case EOF: return p_pull(l, p, nil);
    default: return p_ungetc(*p, c),
      push1(l, (word) p_rx2k) ? p_rxr(l, p) : p_pull(l, p, 0); } }

static word p_rxr(state l, P **p) {
  int c = p_rxc(*p); switch (c) {
    case ')': case EOF: return p_pull(l, p, 0);
    case '(': return p_rx2(l, p);
    case '"': return p_pull(l, p, (word) p_rx_lit_str(l, p));
    case '\'': return push1(l, (word) p_rx_q_cont) ? p_rxr(l, p) : p_pull(l, p, 0);
    default: return p_ungetc(*p, c), p_rx_a(l, p); } }

static enum status p_rx(state l, P **p) {
  word x = p_rxr(l, p);
  return x && push1(l, x) ? Ok : Oom; }

enum status rx_cstr(state l, const char *in) {
  P *p = p_cstr(l, in);
  if (!p || !push1(l, (word) p)) return Oom;
  p = (P*) l->sp[0], l->sp[0] = (word) p_rx_ret;
  word x; avec(l, p, x = p_rxr(l, &p));
  return !x || !push1(l, x) ? Oom : Ok; }
