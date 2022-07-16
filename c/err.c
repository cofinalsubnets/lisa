#include "la.h"
#include <stdarg.h>

Vm(oom_err) { return Pack(),
  err(v, 0, "oom with %d words", v->len); }

Ll(dom_err) { return Pack(),
  err(v, 0, "is undefined"); }

Ll(ary_err) { return Pack(),
  err(v, 0, "takes %d arguments", getZ(xp)); }

// type/arity checking
#define DTc(n, t) Vm(n) {\
  return IsA(t, xp) ? ApN(1, xp) : ApC(ary_err, xp); }
DTc(idZ, Num)
DTc(idH, Hom)
DTc(idT, Tbl)
DTc(id2, Two)
Vm(arity) {
  ob reqd = (ob) ip[1].ll;
  return reqd > fp->argc ?
    ApC(ary_err, reqd) :
    ApN(2, xp); }

static u0 show_call(ps v, mo ip, co fp) {
  fputc('(', stderr);
  tx(v, stderr, (ob) ip);
  for (uintptr_t i = 0, argc = getnum(fp->argc); i < argc;)
    fputc(' ', stderr),
    tx(v, stderr, fp->argv[i++]);
  fputc(')', stderr); }

static Inline u1 atop(pt v, co fp) {
  return ptr(fp) == v->pool + v->len; }

static ob restart(pt v) {
  v->fp = (fr) (v->pool + v->len);
  v->sp = (ob*) v->fp;
  v->xp = nil;
  v->ip = (dt) nil;
  return 0; }

NoInline ob err(pt v, ob x, const char *msg, ...) {
  if (x || msg) {
    dt ip = v->ip;
    fr fp = v->fp;
    // error line
    fputs("# ", stderr);
    if (!atop(v, fp)) show_call(v, ip, fp), fputs(" ", stderr);
    if (msg) {
      va_list xs;
      va_start(xs, msg);
      vfprintf(stderr, msg, xs);
      va_end(xs);
      if (x) fputs(" ", stderr); }
    if (x) tx(v, stderr, x);
    fputc('\n', stderr);

    // backtrace
    if (!atop(v, fp)) for (;;) {
      ip = (dt) fp->retp, fp = (fr)
        ((ob*) (fp + 1) + getnum(fp->argc)
                        + getnum(fp->subd));
      if (atop(v, fp)) break; else
        fputs("# in ", stderr),
        show_call(v, ip, fp),
        fputc('\n', stderr); } }

  // reset and yield
  return restart(v); }
