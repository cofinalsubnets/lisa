#include "i.h"


void generic_print(core f, FILE *o, word x) {
  fprintf(o, "#%lx", x); }

void transmit(core f, FILE* o, word x) {
  if (nump(x)) fprintf(o, "%ld", getnum(x));
  else if (R(x)->ap == data) R(x)[1].typ->emit(f, o, x);
  else generic_print(f, o, x); }

static const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
#define DEFAULT_NUMBER_OUTPUT_BASE 10
static void tx_num_r(core f, output o, int base, intptr_t n) {
  ldiv_t qr = ldiv(n, base);
  if (qr.quot) tx_num_r(f, o, base, qr.quot);
  o->putc(f, o, digits[qr.rem]); }

void tx_num(core v, output o, int base, intptr_t n) {
  if (!n) o->putc(v, o, '0');
  else tx_num_r(v, o, base, n); }
