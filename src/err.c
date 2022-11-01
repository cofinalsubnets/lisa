#include "la.h"

// errors
Vm(xdom) { return Pack(), nope(v, "is undefined"); }
Vm(xoom) { return Pack(), nope(v, "oom with %d words", v->len); }
Vm(xary) { return Pack(), nope(v, "takes %d parameters", getnum(xp)); }

#define aubas (((ob*) fp) == v->pool + v->len)
static NoInline void show_call(la v, mo ip, fr fp) {
  fputc('(', stderr);
  la_tx(v, stderr, (ob) ip);
  for (size_t i = 0, argc = fp->argc; i < argc;
    fputc(' ', stderr), la_tx(v, stderr, fp->argv[i++]));
  fputc(')', stderr); }

#include <stdarg.h>
static void verrp(la v, const char *msg, va_list xs) {
  mo ip = v->ip;
  fr fp = v->fp;

  // print error
  fputs("# ", stderr);
  // show the function if there is one
  if (!aubas) show_call(v, ip, fp), fputc(' ', stderr);
  // show message
  vfprintf(stderr, msg, xs), fputc('\n', stderr);

  // show backtrace
  while (!aubas)
    fputs("# in ", stderr),
    show_call(v, ip, fp),
    fputc('\n', stderr),
    ip = (mo) fp->retp,
    fp = fp->subd; }

void errp(la v, const char *msg, ...) {
  va_list xs;
  va_start(xs, msg), verrp(v, msg, xs), va_end(xs); }

NoInline ob nope(la v, const char *msg, ...) {
  if (msg) {
    va_list xs;
    va_start(xs, msg), verrp(v, msg, xs), va_end(xs); }
  v->fp = (fr) (v->sp = v->pool + v->len);
  v->ip = (mo) (v->xp = nil);
  return 0; }
