#include "i.h"
typedef word parser(state, source, word);
#define Parse(n) word n(state f, source i, word x)
static Parse(rx_ret) { return x; }
static Inline Parse(pull) { return ((parser*) pop1(f))(f, i, x); }
#define Fail() pull(f, i, 0)

#define Getc getc
#define Ungetc ungetc
#define Feof feof

static string
  string_literal(state, source);

static word
  rxr(state, source),
  list(state, source),
  atom(state, source);


// FIXME should distinguish between OOM and parse error
enum status read_source(state f, source i) {
  word x; return
    !push1(f, (word) rx_ret) ? Oom :
    !(x = rxr(f, i)) ? Feof(i) ? Eof : Dom :
    push1(f, x) ? Ok : Oom; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

// get the next token character from the stream
static NoInline int rx_char(source i) {
  for (int c;;) switch (c = Getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (Getc(i)) {
      case '\n': case EOF: return rx_char(i); } } }

static Parse(list_cons) {
  word y = pop1(f);
  return pull(f, i, x ? (word) cons(f, y, x) : x); }

static Parse(list_cont) { return
  x && push2(f, (word) list_cons, x) ? list(f, i) : Fail(); }

static Parse(rx_q_cont) { return
  pull(f, i, x ? (word) cons(f, x, nil) : x); }

static NoInline word rxr(state f, source i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(f, i, 0);
    case '(': return list(f, i);
    case '"': return pull(f, i, (word) string_literal(f, i));
    case '\'': return push1(f, (word) rx_q_cont) ? rxr(f, i) : Fail();
    default:
      Ungetc(c, i);
      return atom(f, i); } }

static word list(state f, source i) {
  int c = rx_char(i); switch (c) {
    case ')': case EOF: return pull(f, i, nil);
    default:
      Ungetc(c, i);
      return push1(f, (word) list_cont) ?  rxr(f, i) : Fail(); } }

static NoInline string string_literal(state f, source i) {
  string o = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // backslash causes the next character
      // to be read literally // TODO more escape sequences
      case '\\': if ((x = Getc(i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, o; }
  return 0; }

static NoInline word atom(state f, source i) {
  string a = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); a; a = buf_grow(f, a), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': Ungetc(x, i);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = x; continue; } out:
  if (!a) return Fail();
  char *e; long n = strtol(a->text, &e, 0);
  return pull(f, i, *e == 0 ? putnum(n) : (word) a); }
