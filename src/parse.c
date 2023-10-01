#include "i.h"
typedef word parser(state, source, word, status);
#define Parse(n) word n(state f, source i, word x, status s)
static Parse(rx_ret) { return x; }
static Inline Parse(pull) { return ((parser*) pop1(f))(f, i, x, s); }

#define Getc getc
#define Ungetc ungetc
#define Feof feof

static int read_char(source);
static parser p_xs_cont, p_xs_cons;

static word
  p_x(state, source),
  p_xs(state, source),
  read_str_lit(state, source),
  read_atom(state, source);

// push parser continuations
static Inline word ppk(state f, parser *k) {
  return push1(f, (word) k); }
static Inline word ppkx(state f, parser *k, word x) {
  return push2(f, (word) k, x); }

// FIXME should distinguish between OOM and parse error
enum status read_source(state f, source i) {
  word x; return
    !ppk(f, rx_ret) ? Oom :
    !(x = p_x(f, i)) ? Feof(i) ? Eof : Dom :
    push1(f, x) ? Ok : Oom; }

////
/// " the parser "
//
// simple except it uses the managed stack for recursion.

static NoInline word p_x(state f, source i) {
  int c = read_char(i); switch (c) {
    case ')': case EOF: return pull(f, i, 0, Ok);
    case '(': return p_xs(f, i);
    case '"': return pull(f, i, read_str_lit(f, i), Ok);
    default:
      Ungetc(c, i);
      return pull(f, i, read_atom(f, i), Ok); } }

static word p_xs(state f, source i) {
  int c = read_char(i); switch (c) {
    case ')': case EOF:
      return pull(f, i, nil, Ok);
    default:
      Ungetc(c, i);
      return ppk(f, p_xs_cont) ?
        p_x(f, i) :
        pull(f, i, 0, Ok); } }

static Parse(p_xs_cons) {
  word y = pop1(f);
  x = x ? (word) cons(f, y, x) : x;
  return pull(f, i, x, Ok); }

static Parse(p_xs_cont) { return
  x && ppkx(f, p_xs_cons, x) ?
    p_xs(f, i) :
    pull(f, i, 0, Ok); }

static NoInline word read_str_lit(state f, source i) {
  string o = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // backslash escapes next character
      case '\\': if ((x = Getc(i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin:
        o->len = n;
        return (word) o; }
  return 0; }

static NoInline word read_atom(state f, source i) {
  string a = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); a; a = buf_grow(f, a), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '\'': case '"': Ungetc(x, i);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = x; continue; } out:
  if (!a) return 0;
  char *e;
  long n = strtol(a->text, &e, 0);
  return *e == 0 ? putnum(n) : (word) a; }

// get the next token character from the stream
static NoInline int read_char(source i) {
  for (int c;;) switch (c = Getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (Getc(i)) {
      case '\n': case EOF: return read_char(i); } } }
