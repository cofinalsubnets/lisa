#include "la.h"
#include "tx.h"

// errors
Vm(xary) { return Pack(), LA_XARY; }
Vm(xnom) { return Pack(), LA_XNOM; }
Vm(xdom) { return Pack(), LA_XDOM; }
Vm(xoom) { return Pack(), LA_XOOM; }

#include <string.h>
#include <errno.h>
#include <stdarg.h>

#define aubas (((ob*) fp) == v->pool + v->len)
static NoInline void show_call(la v, mo ip, sf fp) {
  fputc('(', stderr);
  la_tx(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    fputc(' ', stderr), la_tx(v, stderr, fp->argv[i++]));
  fputc(')', stderr); }

static NoInline void errp(la v, const char *msg, ...) {
  mo ip = v->ip;
  sf fp = v->fp;

  // print error
  fputs("# ", stderr);

  // show the function if there is one
  if (!aubas)
    show_call(v, ip, fp),
    fputc(' ', stderr),
    ip = fp->retp,
    fp = fp->subd;

  // show message
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  fputc('\n', stderr);

  // show backtrace
  while (!aubas)
    fputs("# in ", stderr),
    show_call(v, ip, fp),
    fputc('\n', stderr),
    ip = (mo) fp->retp,
    fp = fp->subd; }

void la_perror(la_carrier v, la_status s) {
  switch (s) {
    // not error codes, so print nothing.
    case LA_OK: case LA_EOF: return;
    case LA_XDOM: errp(v, "has no value"); break;
    case LA_XOOM: errp(v, "oom at %d words", v->len); break;
    case LA_XSYN: errp(v, "syntax error"); break; // TODO source info
    case LA_XARY:
      errp(v, "wrong arity : %d of %d", v->fp->argc, getnum(v->xp));
      break;
    case LA_XNOM: {
      const char *n = "#sym";
      size_t l = 4;
      str s = ((sym) v->xp)->nom;
      if (s) n = s->text, l = s->len;
      errp(v, "free variable : %.*s", l, n); 
      break; }
    case LA_XSYS:
      errp(v, "system error : %s", strerror(errno)); } }
