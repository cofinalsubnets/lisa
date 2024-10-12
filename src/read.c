#include "i.h"

string new_buffer(core f) {
  string s = cells(f, Width(struct string) + 1);
  return s ? ini_str(s, sizeof(word)) : s; }

NoInline string grow_buffer(core f, string s) {
  string t; size_t len = s->len;
  avec(f, s, t = cells(f, Width(struct string) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

#define Getc getc
#define Ungetc ungetc
#define Feof feof

// get the next significant character from the stream
static NoInline int read_char(core f, input i) {
  for (int c;;) loop: switch (c = i->getc(f, i)) {
    default: return c;
    case ' ': case '\t': case '\n': goto loop;
    case '#': case ';': for (;;) switch (i->getc(f, i)) {
      case '\n': case EOF: goto loop; } } }

static word read_str_lit(core, input),
            read_atom(core, input, int);

static status reads(core, input);
////
/// " the parser "
//
static status enquote(core f) {
  pair w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (word) w;
  symbol y = literal_symbol(f, "`");
  if (!y) return Oom;
  w = pairof(f, (word) y, f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = (word) w;
  return Ok; }

static status read1c(core f, input i, int c) {
  word x; switch (c) {
    case EOF: return Eof;
    case '\'':
      c = read1i(f, i);
      return c == Ok ? enquote(f) : c;
    case '(': return reads(f, i);
    case ')': x = nil; break;
    case '"': x = read_str_lit(f, i); break;
    default: x = read_atom(f, i, c); }
  return x && pushs(f, 1, x) ? Ok : Oom; }

status read1i(core f, input i) {
  return read1c(f, i, read_char(f, i)); }

static int file_getc(core f, input i) {
  return getc((FILE*) i->data[0]); }
static void file_ungetc(core f, input i, char c) {
  ungetc(c, (FILE*) i->data[0]); }
static bool file_eof(core f, input i) {
  return feof((FILE*) i->data[0]); }

status read1(core f, FILE *file) {
  void *in[] = { file_getc, file_ungetc, file_eof, file };
  return read1i(f, (input) in); }

static status reads(core f, input i) {
  word c = read_char(f, i);
  switch (c) {
    case ')': case EOF: unnest:
      return pushs(f, 1, nil) ? Ok : Oom;
    default:
      c = read1c(f, i, c);
      if (c == Eof) goto unnest;
      if (c != Ok) return c;
      c = reads(f, i);
      if (c != Ok) return c;
      c = (word) pairof(f, f->sp[1], f->sp[0]);
      if (!c) return Oom;
      *++f->sp = (word) c;
      return Ok; } }

static NoInline word read_str_lit(core f, input i) {
  string o = new_buffer(f);
  for (size_t n = 0, lim = sizeof(word); o; o = grow_buffer(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = i->getc(f, i)) {
      // backslash escapes next character
      case '\\': if ((x = i->getc(f, i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, (word) o; }
  return 0; }

static NoInline word read_atom(core f, input i, int c) {
  string a = new_buffer(f);
  if (a) a->text[0] = c;
  for (size_t n = 1, lim = sizeof(word); a; a = grow_buffer(f, a), lim *= 2)
    while (n < lim) switch (c = i->getc(f, i)) {
      // these characters terminate an atom
      case ' ': case '\n': case '\t': case ';': case '#':
      case '(': case ')': case '"': case '\'': i->ungetc(f, i, c);
      case EOF: a->text[a->len = n] = 0; goto out;
      default: a->text[n++] = c; continue; } out:
  if (!a) return 0;
  char *e; long n = strtol(a->text, &e, 0);
  return *e == 0 ? putnum(n) : (word) intern(f, a); }
