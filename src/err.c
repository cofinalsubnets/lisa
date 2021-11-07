#include "lips.h"
#include "err.h"
#include "terp.h"
#include "hom.h"
#include "io.h"

static u0 vferrp(FILE* o, char *msg, va_list xs) {
  fputs("# ", o), vfprintf(o, msg, xs), fputc('\n', o); }

u0 errp(lips v, char *msg, ...) {
 va_list xs;
 va_start(xs, msg);
 vferrp(stderr, msg, xs);
 va_end(xs); }

obj err(lips v, char *msg, ...) {
 va_list xs; va_start(xs, msg);
 vferrp(stderr, msg, xs);
 return restart(v); }

obj restart(lips v) {
 v->fp = v->sp = v->pool + v->len;
 v->xp = v->ip = nil;
 v->root = NULL;
 if (v->restart) longjmp(*v->restart, 1);
 abort(); }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline u0 perrarg(lips v, mem fp) {
 i64 i = 0, argc = fp == v->pool + v->len ? 0 : getnum(ARGC);
 if (argc) for (fputc(' ', stderr);;fputc(' ', stderr)) {
  obj x = ARGV[i++];
  emit(v, x, stderr);
  if (i == argc) break; }
 fputc(')', stderr); }

VM(nope, const char *msg, ...) {
 fputs("# (", stderr);
 emit(v, Ph(ip), stderr);
 perrarg(v, fp);
 va_list xs;
 fputs(" : ", stderr);
 va_start(xs, msg); vfprintf(stderr, msg, xs);
 fputc('\n', stderr);
 for (;;) {
  ip = RETP, fp += Size(frame) + getnum(ARGC) + getnum(SUBR);
  if (button(Gh(ip))[-1] == yield) break;
  fputs("# in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
 Hp = hp;
 return restart(v); }
