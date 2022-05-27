#include "la.h"
#include <stdarg.h>

Ll(domain_error) { return Pack(),
  err(v, xp, "is undefined at"); }

Ll(ary_error) { return Pack(),
  err(v, 0, "has %d of %d arguments",
      getnum(fp->argc), getnum(xp)); }

// type/arity checking
#define DTc(n, t) Ll(n) { TypeCheck(xp, t); return ApN(1, xp); }
DTc(idZ, Num) DTc(idH, Hom)
DTc(idT, Tbl) DTc(id2, Two)
Ll(arity) {
  ob reqd = (ob) ip[1].ll;
  return reqd <= fp->argc ?  ApN(2, xp) : ApC(ary_error, reqd); }

static void show_call(em v, yo ip, fr fp) {
  fputc('(', stderr), emit(v, (ob) ip, stderr);
  for (uintptr_t i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr), emit(v, fp->argv[i++], stderr);
  fputc(')', stderr); }

#define atop ((ob*)fp==v->pool+v->len)
NoInline ob err(em v, ob x, const char *msg, ...) {
  if (x || msg) {
    yo ip = v->ip;
    fr fp = v->fp;
    // error line
    fputs("# ", stderr);
    if (!atop) show_call(v, ip, fp), fputs(" ", stderr);
    if (msg) {
      va_list xs;
      va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
      if (x) fputs(" ", stderr); }
    if (x) emit(v, x, stderr);
    fputc('\n', stderr);

    // backtrace
    if (!atop) for (;;) {
      ip = (yo) fp->retp, fp = (fr)
        ((ob*) (fp + 1) + getnum(fp->argc)
                        + getnum(fp->subd));
      if (atop) break; else
        fputs("# in ", stderr),
        show_call(v, ip, fp),
        fputc('\n', stderr); } }
#undef atop

  // reset and yield
  return v->fp = (fr) (v->pool + v->len),
         v->sp = (ob*) v->fp,
         v->xp = nil,
         v->ip = (mo) nil,
         0; }



