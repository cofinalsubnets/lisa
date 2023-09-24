#include "i.h"

enum status l_evals(state f, const char *prog) {
  enum status s = receive2(f, prog);
  return s != Ok ? s : eval(f, pop1(f)); }

void l_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

enum status l_ini(state f) {
  memset(f, 0, sizeof(struct G));
  word *pool = malloc(2 * sizeof(intptr_t));
  if (!pool) return Oom;
  f->loop = f->sp = (f->pool = f->hp = pool) + (f->len = 1);
  f->dict = nil;
  f->t0 = clock();
  return Ok; }
