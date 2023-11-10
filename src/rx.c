#include "i.h"

string buf_new(state f) {
  string s = cells(f, Width(struct string) + 1);
  if (s) s->ap = data, s->typ = String, s->len = sizeof(word);
  return s; }

NoInline string buf_grow(state f, string s) {
  string t; size_t len = s->len;
  avec(f, s, t = cells(f, Width(struct string) + 2 * b2w(len)));
  if (t) t->ap = data, t->typ = String, t->len = 2 * len,
         memcpy(t->text, s->text, len);
  return t; }

static status p1(gwen, source), p0(gwen, source);
typedef status parser(state, source, word, status);
#define Parse(n) status n(state f, source i, word x, status s)
static Parse(rx_ret) { return s ? s : push1(f, x) ? Ok : Oom; }
static Inline Parse(pull) { return ((parser*) pop1(f))(f, i, x, s); }

#define Getc getc
#define Ungetc ungetc
#define Feof feof

// get the next significant character from the stream
static NoInline int read_char(source i) {
  for (int c;;) switch (c = Getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': continue;
    case '#': case ';': for (;;) switch (Getc(i)) {
      case '\n': case EOF: return read_char(i); } } }

static word read_str_lit(state, source),
            read_atom(state, source);

////
/// " the parser "
//


status read_source(state f, source i) {
  return p0(f, i); }

static status p0(gwen f, source i) {
  word x, c = read_char(i); switch (c) {
    case '(': return p1(f, i);
    case ')': case EOF: x = nil; break;
    case '"': x = read_str_lit(f, i); break;
    default: Ungetc(c, i), x = read_atom(f, i); }
  return x && push1(f, x) ? Ok : Oom; }

static status p1(gwen f, source i) {
  for (word m, c, n = 0, d = 0;;) switch ((c = read_char(i))) {
    case '(': // nest
      if (!push1(f, putnum(n + 1))) return Oom;
      else n = 0, d++; continue;
    case ')': case EOF: // unnest
      for (c = nil; n--; m = pop1(f), c = c ? (word) cons(f, m, c) : c);
      if (!c) return Oom;
      else if (d == 0) return push1(f, c) ? Ok : Oom;
      else n = getnum(f->sp[0]), f->sp[0] = c, d--;
      continue;
    default: // read one
      Ungetc(c, i), n++, c = p0(f, i);
      if (c != Ok) return c; } }

static NoInline word read_str_lit(state f, source i) {
  string o = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); o; o = buf_grow(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // backslash escapes next character
      case '\\': if ((x = Getc(i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, (word) o; }
  return 0; }

static NoInline word read_atom(state f, source i) {
  string a = buf_new(f);
  for (size_t n = 0, lim = sizeof(word); a; a = buf_grow(f, a), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '"': Ungetc(x, i);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = x; continue; } out:
  if (!a) return 0;
  char *e;
  long n = strtol(a->text, &e, 0);
  return *e == 0 ? putnum(n) : (word) a; }
