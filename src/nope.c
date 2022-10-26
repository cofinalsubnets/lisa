#include "lisa.h"
#include "vm.h"
#include <stdarg.h>
static NoInline void show_call(la v, mo ip, fr fp) {
  fputc('(', stderr);
  tx(v, stderr, (ob) ip);
  for (size_t i = 0, argc = getnum(fp->argc); i < argc;
    fputc(' ', stderr), tx(v, stderr, fp->argv[i++]));
  fputc(')', stderr); }

#define aubas (((ob*) fp) == v->pool + v->len)
NoInline ob nope(la v, const char *msg, ...) {
  mo ip = v->ip;
  fr fp = v->fp;

  // print error
  fputs("# ", stderr);
  // show the function if there is one
  if (!aubas) show_call(v, ip, fp), fputc(' ', stderr);
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
    fp = (fr) ((ob*) (fp + 1) + getnum(fp->argc)
                              + getnum(fp->subd));
  // reset and yield
  v->fp = (fr) (v->sp = v->pool + v->len);
  v->ip = (mo) (v->xp = nil);
  return 0; }

// errors
Vm(dom_err) { return Pack(),
  nope(v, "is undefined"); }
Vm(oom_err) { return Pack(),
  nope(v, "oom with %d words", v->len); }
Vm(ary_err) { return Pack(),
  nope(v, "takes %d parameters", getnum(xp)); }
