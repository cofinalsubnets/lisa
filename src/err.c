#include "lips.h"
#include "terp.h"
#include "err.h"
#include "hom.h"
#include "io.h"

u0 errp(lips v, char *msg, ...) {
  va_list xs;
  fputs("# ", stderr);
  va_start(xs, msg);
  vfprintf(stderr, msg, xs);
  va_end(xs);
  fputc('\n', stderr); }

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
    ip = RETP, fp += Width(frame) + N(ARGC) + N(SUBR);
    if (button(H(ip))[-1] == yield) break;
    fputs("# in ", stderr), emsep(v, ip, stderr, '\n'); }

  v->hp = hp;
  return restart(v); }

obj restart(lips v) {
  v->fp = v->sp = v->pool + v->len;
  v->xp = v->ip = nil;
  v->root = NULL;
  longjmp(v->restart, 1); }

// errors
VM(fail) { Jump(nope, "fail"); }

VM(type_error) {
 enum tag exp = v->xp, act = kind(xp);
 Jump(nope, "wrong type : %s for %s", tnom(act), tnom(exp)); }

VM(oob_error) {
 Jump(nope, "oob : %d >= %d", v->xp, v->ip); }

// type/arity checking
#define DTC(n, t) VM(n) {\
  if (kind(xp-t)==0) NEXT(1);\
  v->xp = t; Jump(type_error); }
DTC(idZ, Num) DTC(idH, Hom) DTC(idT, Tbl) DTC(id2, Two)
VM(arity) {
 obj reqd = (obj) GF(ip);
 if (reqd <= ARGC) NEXT(2);
 else Jump(nope, arity_err_msg, N(ARGC), N(reqd)); }
