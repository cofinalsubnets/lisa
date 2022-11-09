#include "la.h"
#include "boot.h"
#include "rx.h"
#include "vm.h"
#include "alloc.h"
#include "tbl.h"

// bootstrap eval interpreter function
Vm(ev_f) {
  ArityCheck(1);
  mo y;
  CallOut(y = ana(v, fp->argv[0]));
  return y ? ApY(y, xp) : ApC(xoom, xp); }

// return to C
static Vm(yield) { return Pack(), LA_OK; }
la_status la_call(la_carrier v, la_fn f, size_t n) {
  struct la_fn go[] = { {call}, {(vm*) putnum(n)}, {yield} };
  return call(v, (ob) f, go, v->hp, v->sp, v->fp); }

la_status la_ev_f(la_carrier v, FILE *in) {
  enum la_status s = la_rx_f(v, in);
  if (s != LA_OK) return s;
  if (!pushs(v, v->xp, NULL)) return LA_XOOM;
  mo ev = (mo) tblget(v, v->topl, (ob) v->lex[Eval], 0);
  return la_call(v, ev, 1); }
