#include "la.h"
#include "io.h"
#include "chars.h"
#include <stdarg.h>
static void show_call(pt, mo, fr);
#define bottom (ptr(fp) == v->pool + v->len)

NoInline ob err(pt v, ob x, const char *msg, ...) {
  if (x || msg) {
    dt ip = v->ip;
    fr fp = v->fp;
    // error line
    fputs("# ", stderr);
    if (!bottom)
      show_call(v, ip, fp),
      fputc(Space, stderr);
    va_list xs;
    va_start(xs, msg);
    vfprintf(stderr, msg, xs);
    va_end(xs);
    if (x) fputc(Space, stderr), tx(v, stderr, x);
    fputc(Newline, stderr);

    // backtrace
    while (!bottom)
      fputs("# à ", stderr),
      show_call(v, ip, fp),
      fputc(Newline, stderr),
      ip = (mo) fp->retp,
      fp = (fr) ((ob*) (fp + 1) + getZ(fp->argc)
                                + getZ(fp->subd)); }

  // reset and yield
  return
    v->fp = (fr) (v->pool + v->len),
    v->sp = (ob*) v->fp,
    v->xp = nil,
    v->ip = (mo) nil,
    0; }

static void show_call(pt v, mo ip, fr fp) {
  fputc(LeftParen, stderr);
  tx(v, stderr, (ob) ip);
  for (uintptr_t i = 0, argc = getZ(fp->argc); i < argc;)
    fputc(Space, stderr),
    tx(v, stderr, fp->argv[i++]);
  fputc(RightParen, stderr); }
