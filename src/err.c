#include "la.h"
#include <string.h>
#include <errno.h>
#include <stdarg.h>

u0 unwind(la v) {
  v->sp = v->pool + v->len,
  v->fp = (sf) v->sp,
  v->ip = 0,
  v->xp = nil; }

// errors
Vm(xary) { return Pack(), LA_XARY; }
Vm(xdom) { return Pack(), LA_XDOM; }
Vm(xoom) { return Pack(), LA_XOOM; }

static NoInline u0 report_call(la v, mo ip, sf fp) {
  putc('(', stderr);
  transmit(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    putc(' ', stderr),
    transmit(v, stderr, fp->argv[i++]));
  putc(')', stderr); }

// this prints a backtrace.
// TODO maybe show it upside down like python?
#define aubas (((ob*) fp) == v->pool + v->len)
static NoInline u0 report(la v, const char *msg, ...) {
  mo ip = v->ip;
  sf fp = v->fp;

  // print error
  fputs(";; ", stderr);

  // show the function if there is one
  if (!aubas)
    report_call(v, ip, fp),
    putc(' ', stderr),
    ip = fp->retp,
    fp = fp->subd;

  // show message
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  putc('\n', stderr);

  // show backtrace
  while (!aubas)
    fputs(";; in ", stderr),
    report_call(v, ip, fp),
    putc('\n', stderr),
    ip = (mo) fp->retp,
    fp = fp->subd; }

u0 la_perror(la v, enum status s) { switch (s) {
  // not error codes, so print nothing.
  case LA_OK: case LA_EOF: return;
  case LA_XDOM: report(v, "has no value"); break;
  case LA_XOOM: report(v, "oom at %d words", v->len); break;
  case LA_XSYN: report(v, "syntax error"); break; // TODO source info
  case LA_XARY:
    report(v, "wrong arity : %d of %d", v->fp->argc, getnum(v->xp));
    break;
  case LA_XNOM: {
    const char *n = "#sym";
    U l = 4;
    str s = ((sym) v->xp)->nom;
    if (s) n = s->text, l = s->len;
    report(v, "free variable : %.*s", l, n); 
    break; }
  case LA_XSYS:
    report(v, "system error : %s", strerror(errno)); } }
