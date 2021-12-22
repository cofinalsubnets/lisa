#include "lips.h"
#include "err.h"
#include "terp.h"
#include "hom.h"
#include "io.h"

u0 errp(lips v, char *msg, ...) {
  va_list xs;
  fputs("# ", stderr);
  va_start(xs, msg);
  vfprintf(stderr, msg, xs);
  va_end(xs);
  fputc('\n', stderr); }

obj restart(lips v) {
  v->fp = v->sp = v->pool + v->len;
  v->xp = v->ip = nil;
  v->root = NULL;
  longjmp(v->restart, 1); }

VM(nope, const char *msg, ...) {
  // print current call as (function arg1 arg2 ...)
  fputs("# (", stderr);
  emit(v, ip, stderr);
  mem top = v->pool + v->len;
  i64 i = 0, argc = fp == top ? 0 : N(ARGC);
  if (argc) for (fputc(' ', stderr);; fputc(' ', stderr)) {
    obj x = ARGV[i++];
    emit(v, x, stderr);
    if (i == argc) break; }
  fputc(')', stderr);

  // print error message
  va_list xs;
  fputs(" : ", stderr);
  va_start(xs, msg); vfprintf(stderr, msg, xs);
  fputc('\n', stderr);

  // print backtrace
  for (;;) {
    ip = RETP, fp += Size(frame) + getnum(ARGC) + getnum(SUBR);
    if (button(Gh(ip))[-1] == yield) break;
    fputs("# in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }

  v->hp = hp;
  return restart(v); }

// errors
VM(fail) { Jump(nope, "fail"); }

// type/arity checking
#define TDCN(t) if(!kind(xp-t)){ NEXT(1);}\
 Jump(nope,type_err_msg,tnom(kind(xp)),tnom(t))
#define DTC(n, t) VM(n) { TDCN(t); }
DTC(idZ, Num) DTC(idH, Hom) DTC(idT, Tbl) DTC(id2, Two)
VM(arity) {
 obj reqd = (obj) GF(ip);
 if (reqd <= ARGC) { NEXT(2); }
 Jump(nope, arity_err_msg, N(ARGC), N(reqd)); }
