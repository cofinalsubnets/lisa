#include "la.h"
#include <stdarg.h>

Vm(oom_err) { return Pack(),
  err(v, 0, "oom with %d words", v->len); }

Ll(dom_err) { return Pack(),
  err(v, 0, "is undefined"); }

Ll(ary_err) { return Pack(),
  err(v, 0, "takes %d arguments", getZ(xp)); }

// type/arity checking
#define DTc(n, t) Vm(n) { Typ(xp, t); return ApN(1, xp); }
DTc(idZ, Num)
DTc(idH, Hom)
DTc(idT, Tbl)
DTc(id2, Two)
Ll(arity) {
  ob reqd = (ob) ip[1].ll;
  return reqd > fp->argc ?
    ApC(ary_err, reqd) :
    ApN(2, xp); }

static void show_call(ps v, mo ip, co fp) {
  fputc('(', stderr), emit(v, (ob) ip, stderr);
  for (uintptr_t i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr), emit(v, fp->argv[i++], stderr);
  fputc(')', stderr); }

static Inline bool atop(ps v, co fp) {
  return R(fp) == v->pool + v->len; }

NoInline ob err(em v, ob x, const char *msg, ...) {
  if (x || msg) {
    mo ip = v->ip;
    fr fp = v->fp;
    // error line
    fputs("# ", stderr);
    if (!atop(v, fp)) show_call(v, ip, fp), fputs(" ", stderr);
    if (msg) {
      va_list xs;
      va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
      if (x) fputs(" ", stderr); }
    if (x) emit(v, x, stderr);
    fputc('\n', stderr);

    // backtrace
    if (!atop(v, fp)) for (;;) {
      ip = (mo) fp->retp, fp = (fr)
        ((ob*) (fp + 1) + getnum(fp->argc)
                        + getnum(fp->subd));
      if (atop(v, fp)) break; else
        fputs("# in ", stderr),
        show_call(v, ip, fp),
        fputc('\n', stderr); } }

  // reset and yield
  return v->fp = (fr) (v->pool + v->len),
         v->sp = (ob*) v->fp,
         v->xp = nil,
         v->ip = (mo) nil,
         0; }
