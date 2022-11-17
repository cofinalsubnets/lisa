#include "la.h"
#include <string.h>
#include <errno.h>
#include <stdarg.h>

// errors
Vm(xary) { return Pack(), LA_XARY; }
Vm(xdom) { return Pack(), LA_XDOM; }
Vm(xoom) { return Pack(), LA_XOOM; }

static NoInline void show_call(la v, FILE *o, mo ip, sf fp) {
  fputc('(', o);
  la_tx(v, o, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    fputc(' ', o), la_tx(v, o, fp->argv[i++]));
  fputc(')', o); }

// this prints a backtrace.
// TODO maybe do it upside down like python?
#define aubas (((ob*) fp) == v->pool + v->len)
static NoInline void errp(la v, FILE *o, const char *msg, ...) {
  mo ip = v->ip;
  sf fp = v->fp;

  // print error
  fputs(";; ", o);

  // show the function if there is one
  if (!aubas)
    show_call(v, o, ip, fp),
    fputc(' ', o),
    ip = fp->retp,
    fp = fp->subd;

  // show message
  va_list xs;
  va_start(xs, msg), vfprintf(o, msg, xs), va_end(xs);
  fputc('\n', o);

  // show backtrace
  while (!aubas)
    fputs(";; in ", o),
    show_call(v, o, ip, fp),
    fputc('\n', o),
    ip = (mo) fp->retp,
    fp = fp->subd; }

void la_perror(la_carrier v, la_status s, FILE *o) {
  switch (s) {
    // not error codes, so print nothing.
    case LA_OK: case LA_EOF: return;
    case LA_XDOM: errp(v, o, "has no value"); break;
    case LA_XOOM: errp(v, o, "oom at %d words", v->len); break;
    case LA_XSYN: errp(v, o, "syntax error"); break; // TODO source info
    case LA_XARY:
      errp(v, o, "wrong arity : %d of %d", v->fp->argc, getnum(v->xp));
      break;
    case LA_XNOM: {
      const char *n = "#sym";
      size_t l = 4;
      str s = ((sym) v->xp)->nom;
      if (s) n = s->text, l = s->len;
      errp(v, o, "free variable : %.*s", l, n); 
      break; }
    case LA_XSYS:
      errp(v, o, "system error : %s", strerror(errno)); } }
