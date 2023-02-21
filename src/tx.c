#include "i.h"

typedef void emitter(li, FILE*, ob);
static emitter
  tx_two, tx_tbl, tx_str, tx_sym, tx_mo_nom,
  *const emit_data[] = {
    [Two] = tx_two, [Str] = tx_str,
    [Sym] = tx_sym, [Tbl] = tx_tbl, };

void transmit(li v, FILE* o, ob x) {

  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (G(x) == act) emit_data[gettyp(x)](v, o, x);
  else tx_mo_nom(v, o, hnom(v, (mo) x)); }

static void tx_tbl(li v, FILE *o, ob x) {
  fprintf(o, "#tbl:%ld/%ld", ((tbl)x)->len, ((tbl)x)->cap); }

static void tx_sym(li v, FILE *o, ob _) {
  str s = ((sym) _)->nom;
  if (s) fwrite(s->text, 1, s->len, o);
  else fputs("#sym", o); }

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

Vm(txc_f) { return
  !fp->argc ?  Yield(ArityError, putnum(1)) :
  ApC(ret, putnum(putc(getnum(fp->argv[0]), stdout))); }

Vm(tx_f) {
  size_t i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      transmit(v, stdout, fp->argv[i++]),
      putc(' ', stdout);
    xp = fp->argv[i];
    transmit(v, stdout, xp); }
  return putc('\n', stdout),
         ApC(ret, xp); }

static NoInline void tx_mo_nom(li v, FILE *o, ob x) {
  if (symp(x)) putc('\\', o), transmit(v, o, x);
  else if (!twop(x)) putc('\\', o);
  else {
    if (symp(A(x)) || twop(A(x))) tx_mo_nom(v, o, A(x));
    if (symp(B(x)) || twop(B(x))) tx_mo_nom(v, o, B(x)); } }
