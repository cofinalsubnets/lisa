#include "i.h"

string new_buffer(state f) {
  string s = cells(f, Width(struct string) + 1);
  if (s) s->ap = data, s->typ = String, s->len = sizeof(word);
  return s; }

NoInline string grow_buffer(state f, string s) {
  string t; size_t len = s->len;
  avec(f, s, t = cells(f, Width(struct string) + 2 * b2w(len)));
  if (t) t->ap = data, t->typ = String, t->len = 2 * len,
         memcpy(t->text, s->text, len);
  return t; }

#define Getc getc
#define Ungetc ungetc
#define Feof feof

static Inline int igetc(input i) { return i->getc(i); }
static Inline int iungetc(input i, int c) { return i->ungetc(i, c); }
static Inline int ieof(input i) { return i->feof(i); }

// get the next significant character from the stream
static NoInline int read_char(source i) {
  for (int c;;) loop: switch (c = Getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': goto loop;
    case '#': case ';': for (;;) switch (Getc(i)) {
      case '\n': case EOF: goto loop; } } }

static word read_str_lit(state, source),
            read_atom(state, source);

////
/// " the parser "
//

status read1(state f, source i) {
  word x, c = read_char(i); switch (c) {
    case EOF: return Eof;
    case '\'':
      if ((c = read1(f, i)) != Ok) return c;
      x = (word) cons(f, pop1(f), nil);
      if (!x || !pushs(f, 1, x)) return Oom;
      return Ok;
    case '(': return reads(f, i);
    case ')': x = nil; break;
    case '"': x = read_str_lit(f, i); break;
    default: Ungetc(c, i), x = read_atom(f, i); }
  return x && pushs(f, 1, x) ? Ok : Oom; }

status reads(state f, source i) {
  word c = read_char(i);
  switch (c) {
    case ')': case EOF: unnest:
      return pushs(f, 1, nil) ? Ok : Oom;
    default:
      Ungetc(c, i);
      c = read1(f, i);
      if (c == Eof) goto unnest;
      if (c != Ok) return c;
      c = reads(f, i);
      if (c != Ok) return c;
      c = (word) cons(f, f->sp[1], f->sp[0]);
      if (!c) return Oom;
      *++f->sp = (word) c;
      return Ok; } }

static NoInline word read_str_lit(state f, source i) {
  string o = new_buffer(f);
  for (size_t n = 0, lim = sizeof(word); o; o = grow_buffer(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // backslash escapes next character
      case '\\': if ((x = Getc(i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, (word) o; }
  return 0; }

static NoInline word read_atom(state f, source i) {
  string a = new_buffer(f);
  for (size_t n = 0, lim = sizeof(word); a; a = grow_buffer(f, a), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '"': case '\'': Ungetc(x, i);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = x; continue; } out:
  if (!a) return 0;
  char *e; long n = strtol(a->text, &e, 0);
  return *e == 0 ? putnum(n) : (word) a; }

#define ff(i) ((FILE*)i->data[0])
static int std_getc(input i) { return fgetc(ff(i)); }
static int std_ungetc(input i, int c) { return ungetc(c, ff(i)); }
static int std_feof(input i) { return feof(ff(i)); }
static int std_putc(output o, int c) { return fputc(c, ff(o)); }

input std_input(state f, FILE *s) { return
  (input) thd(f, 4, std_getc, std_ungetc, std_feof, s); }
output std_output(state f, FILE *s) { return
  (output) thd(f, 2, std_putc, s); }
