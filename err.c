#include "lips.h"
#include "err.h"

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
