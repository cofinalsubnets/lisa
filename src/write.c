#include "i.h"

static const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
#define DEFAULT_NUMBER_OUTPUT_BASE 10
static void print_num_r(core f, output o, intptr_t n, int base) {
  ldiv_t qr = ldiv(n, base);
  if (qr.quot) print_num_r(f, o, qr.quot, base);
  o->putc(f, o, digits[qr.rem]); }

void print_num(core v, output o, intptr_t n, int base) {
  if (!n) return o->putc(v, o, '0');
  intptr_t abs_n = n < 0 ? -n : n;
  print_num_r(v, o, abs_n, base);
  if (n < 0) o->putc(v, o, '-'); }

Vm(prc) {
  return Do(ip = (thread) sp[1],
            std_output.putc(f, &std_output, getnum(sp[1] = sp[0])),
            sp++); }

Vm(print) {
  transmit(f, &std_output, *sp), puts("");
  return op(1, *sp); }
void outputs(core f, output o, const char *s) {
  while (*s) o->putc(f, o, *s++); }

void transmit(core f, output out, word x) {
  if (nump(x)) print_num(f, out, getnum(x), 10);
  else if (R(x)->ap == data) R(x)[1].typ->emit(f, out, x);
  else out->putc(f, out, '#'), print_num(f, out, x, 16); }
