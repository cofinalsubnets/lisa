#include "i.h"

typedef void emitter(state, FILE*, word);
static emitter tx_two, tx_str;
static emitter *ems[] = { [Pair] = tx_two, [String] = tx_str, };

void transmit(state v, FILE* o, word x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (datp((verb) x)) ems[ptr(x)[1].x](v, o, x);
  else fprintf(o, "#%lx", x); }

static void tx_str(state v, FILE *o, word _) {
  string s = (string) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static void tx_two(state v, FILE *o, word x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }
