#include "i.h"

typedef void emitter(state, FILE*, word);
static emitter tx_two, tx_str;
static emitter *ems[] = { [Pair] = tx_two, [String] = tx_str, };

void transmit(state v, FILE* o, word x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (ptr(x)->ap == data) ems[ptr(x)[1].x](v, o, x);
  else fprintf(o, "#%lx", x); }

static bool atomp(string s) {
  const char cc[] = " \n\t;#()\"'";
  for (size_t i = 0; i < s->len; i++)
    for (const char *c = cc; *c; c++)
      if (s->text[i] == *c) return false;
  return true; }

static void tx_str(state v, FILE *o, word _) {
  string s = (string) _;
  size_t len = s->len;
  if (atomp(s)) for (size_t l = 0; l < len; l++) putc(s->text[l], o);
  else {
    const char *text = s->text;
    putc('"', o);
    for (char c; len--; putc(c, o))
      if ((c = *text++) == '\\' || c == '"') putc('\\', o);
    putc('"', o); } }

static void tx_two(state v, FILE *o, word x) {
  if (!twop(B(x))) putc('\'', o), transmit(v, o, A(x));
  else for (putc('(', o);; putc(' ', o)) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }

status report(state f, status s) {
  switch (s) {
    case Dom:
      fprintf(stderr, "# domain error at [0x%lx]\n", f->ip->x); return s;
    case Oom:
      fprintf(stderr, "# out of memory at %ld words\n", f->len);
    default: return s; } }
