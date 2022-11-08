#include "la.h"
#include "tx.h"
#include "mo.h"
#include "str.h"
#include "lexicon.h"
#include <string.h>

static long tx_mo_n(la, FILE*, ob);

// s-expression writer
long la_tx(la v, FILE *o, ob x) {
  if (nump(x)) return fprintf(o, "%ld", getnum(x));
  if (G(x) == disp) return ((mtbl) GF(x))->emit(v, o, x);
  if (primp((mo) x)) return
    fprintf(o, LA_LEX_LAMBDA "%s", ((struct la_prim*)x)->nom);
  return tx_mo_n(v, o, hnom(v, (mo) x)); }

long fputstr(FILE *o, str s) {
  long i = 0, r = s->len;
  while (i < r) if (fputc(s->text[i++], o) == EOF) return -1;
  return r; }

// FIXME this is really weird
// print a function name
#include "two.h"
static NoInline long tx_mo_n(la v, FILE *o, ob x) {
  if (symp(x)) {
    if (fputs(LA_LEX_LAMBDA, o) == EOF) return -1;
    long r = la_tx(v, o, x);
    return r < 0 ? r : r + sizeof(LA_LEX_LAMBDA)-1; }
  if (!twop(x))
    return fputs(LA_LEX_LAMBDA, o) == EOF ? -1 :
      sizeof(LA_LEX_LAMBDA)-1;
  long r = 0, a;
  // FIXME this is weird
  if (symp(A(x)) || twop(A(x))) {
    a = tx_mo_n(v, o, A(x));
    if (a < 0) return a;
    r += a; }
  if (symp(B(x)) || twop(B(x))) {
    a = tx_mo_n(v, o, B(x));
    if (a < 0) return a;
    r += a; }
  return r; }

#include "vm.h"
Vm(tx_f) {
  size_t i = 0, l = fp->argc;
  if (l) {
    while (i < l - 1)
      la_tx(v, stdout, fp->argv[i++]),
      fputc(LA_CH_SPACE, stdout);
    xp = fp->argv[i];
    la_tx(v, stdout, xp); }
  fputc(LA_CH_ENDL, stdout);
  return ApC(ret, xp); }

Vm(txc_f) {
  ArityCheck(1);
  return ApC(ret,
    putnum(fputc(getnum(fp->argv[0]), stdout))); }
