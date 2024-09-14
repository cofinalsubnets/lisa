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
static NoInline int read_char(source i) {
  for (int c;;) loop: switch (c = Getc(i)) {
    default: return c;
    case ' ': case '\t': case '\n': goto loop;
    case '#': case ';': for (;;) switch (Getc(i)) {
      case '\n': case EOF: goto loop; } } }

static word read_str_lit(core, source),
            read_atom(core, source);

static status reads(core, source);
////
/// " the parser "
//
static status enquote(core f) {
  pair w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (word) w;
  string y = literal_string(f, "`");
  if (!y) return Oom;
  w = pairof(f, (word) y, f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = (word) w;
  return Ok; }

status read1(core f, source i) {
  word x, c = read_char(i); switch (c) {
    case EOF: return Eof;
    case '\'':
      c = read1(f, i);
      return c == Ok ? enquote(f) : c;
    case '(': return reads(f, i);
    case ')': x = nil; break;
    case '"': x = read_str_lit(f, i); break;
    default: Ungetc(c, i), x = read_atom(f, i); }
  return x && pushs(f, 1, x) ? Ok : Oom; }

static status reads(core f, source i) {
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
      c = (word) pairof(f, f->sp[1], f->sp[0]);
      if (!c) return Oom;
      *++f->sp = (word) c;
      return Ok; } }

static NoInline word read_str_lit(core f, source i) {
  string o = new_buffer(f);
  for (size_t n = 0, lim = sizeof(word); o; o = grow_buffer(f, o), lim *= 2)
    for (int x; n < lim;) switch (x = Getc(i)) {
      // backslash escapes next character
      case '\\': if ((x = Getc(i)) == EOF) goto fin;
      default: o->text[n++] = x; continue;
      case '"': case EOF: fin: return o->len = n, (word) o; }
  return 0; }

static NoInline word read_atom(core f, source i) {
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
