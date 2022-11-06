#include "la.h"


static ob nope(la v) { return la_reset(v), 0; }

// errors
Vm(xdom) { return
  Pack(), errp(v, "has no value"), nope(v); }
Vm(xoom) { return
  Pack(), errp(v, "oom with %d words", v->len), nope(v); }
Vm(xary) { return
  Pack(), errp(v, "takes %d parameters", getnum(xp)), nope(v); }
Vm(xnom) {
  Pack();
  const char *n = "#sym";
  size_t l = 4;
  str s = ((sym) xp)->nom;
  if (s) n = s->text, l = s->len;
  errp(v, "free variable : %.*s", l, n);
  return nope(v); }

#define aubas (((ob*) fp) == v->pool + v->len)
static void show_call(la v, mo ip, sf fp) {
  fputc('(', stderr);
  la_tx(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    fputc(' ', stderr), la_tx(v, stderr, fp->argv[i++]));
  fputc(')', stderr); }

#include <stdarg.h>
void errp(la v, const char *msg, ...) {
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
