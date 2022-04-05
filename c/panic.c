#include "lips.h"
#include "terp.h"
#include "mem.h"
#include "write.h"
#include "hom.h"

// errors
Vm(fail) {
  return Pack(), err(v, "fail"); }

#define type_err_msg "wrong type : %s for %s"
Vm(type_error) {
  enum tag exp = v->xp, act = kind(xp);
  return Pack(), err(v, type_err_msg, tnom(act), tnom(exp)); }

Vm(oob_error) {
  i64 a = v->xp, b = v->ip;
  return Pack(), err(v, "oob : %d >= %d", a, b); }

#define arity_err_msg "wrong arity : %d of %d"
Vm(ary_error) {
  i64 a = N(Argc), b = v->xp;
  return Pack(), err(v, arity_err_msg, a, b); }

Vm(div_error) { return Pack(), err(v, "/ 0"); }

// type/arity checking
#define DTc(n, t) Vm(n) {\
  if (kind(xp-t)==0) Next(1);\
  v->xp = t; Jump(type_error); }
DTc(idZ, Num) DTc(idH, Hom) DTc(idT, Tbl) DTc(id2, Two)
Vm(arity) {
  obj reqd = (obj) H(ip)[1];
  if (reqd <= Argc) Next(2);
  else Jump((v->xp = N(reqd), ary_error)); }

static Inline u0 show_call(lips v, obj ip, mem fp) {
  fputc('(', stderr);
  emit(v, ip, stderr);
  mem top = v->pool + v->len;
  for (i64 i = 0, argc = fp == top ? 0 : N(Argc); i < argc;)
    fputc(' ', stderr), emit(v, Argv[i++], stderr);
  fputc(')', stderr); }

NoInline obj err(lips v, const char *msg, ...) {
  obj ip = v->ip, *fp = v->fp;
  fputs("# ", stderr), show_call(v, ip, fp), fputs(" : ", stderr);
  va_list xs;
  va_start(xs, msg), vfprintf(stderr, msg, xs), va_end(xs);
  fputc('\n', stderr);
  // print backtrace
  for (mem top = v->pool + v->len;;) {
    ip = Retp, fp += Width(frame) + N(Argc) + N(Subr);
    if (fp == top) break;
    fputs("#  in ", stderr);
    show_call(v, ip, fp);
    fputc('\n', stderr); }
  v->fp = v->sp = v->pool + v->len;
  v->xp = v->ip = nil;
  return 0; }
