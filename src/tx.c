#include "i.h"

typedef void emitter(li, FILE*, ob);
static emitter
  tx_two, tx_str,
  *const emit_data[] = {
    [Two] = tx_two, [Str] = tx_str, };

void transmit(li v, FILE* o, ob x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == act) emit_data[gettyp(x)](v, o, x);
  else fprintf(o, "#ob@0x%lx", x); }


static void tx_str(li v, FILE *o, ob _) {
  str s = (str) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static void tx_two(li v, FILE *o, ob x) {
  for (putc('(', o);; putc(' ', o)) {
    transmit(v, o, A(x));
    if (!twop(x = B(x))) { putc(')', o); break; } } }
