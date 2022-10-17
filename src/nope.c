#include "la.h"
#include <stdarg.h>
static void show_call(la v, mo ip, fr fp) {
  fputc('(', stderr);
  tx(v, stderr, (ob) ip);
  for (size_t i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr),
    tx(v, stderr, fp->argv[i++]);
  fputc(')', stderr); }

#define bottom (ptr(fp) == v->pool + v->len)
NoInline ob nope(la v, const char *msg, ...) {
  mo ip = v->ip;
  fr fp = v->fp;

  // print error
  fputs("# ", stderr);
  if (!bottom) // show call if possible
    show_call(v, ip, fp),
    fputc(' ', stderr);

  // show message
  va_list xs;
  va_start(xs, msg);
  vfprintf(stderr, msg, xs);
  va_end(xs);
  fputc('\n', stderr);

  // show backtrace
  while (!bottom)
    fputs("# in ", stderr),
    show_call(v, ip, fp),
    fputc('\n', stderr),
    ip = (mo) fp->retp,
    fp = (fr) ((ob*) (fp + 1) + getnum(fp->argc)
                              + getnum(fp->subd));
  // reset and yield
  return
    v->fp = (fr) (v->pool + v->len),
    v->sp = (ob*) v->fp,
    v->xp = nil,
    v->ip = (mo) nil,
    0; }
