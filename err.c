#include "lips.h"

void vferrp(vm v, FILE *o, const char *seg, obj x, const char *msg, va_list xs) {
  if (seg) fputc('[', o), fputs(seg, o), fputs("] ", o);
  if (x) emsep(v, x, o, ' '), fputs(";; ", o);
  vfprintf(o, msg, xs), fputc('\n', o); }

void errp(vm v, const char *seg, obj x, const char *msg, ...) {
  va_list xs;
  va_start(xs, msg);
  vferrp(v, stderr, seg, x, msg, xs);
  va_end(xs); }

obj err(vm v, const char *seg, obj x, const char *msg, ...) {
  va_list xs;
  va_start(xs, msg);
  vferrp(v, stderr, seg, x, msg, xs);
  return restart(v); }

obj restart(vm v) {
  v->fp = v->sp = v->mem_pool + v->mem_len;
  v->xp = v->ip = nil;
  v->mem_root = NULL;
  longjmp(v->restart, 1); }
