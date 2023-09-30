#include "i.h"

void l_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

enum status l_ini(state f) {
  memset(f, 0, sizeof(struct lisa));
  const size_t len = 1;
  word *pool = malloc(2 * len * sizeof(word));
  if (!pool) return Oom;
  f->len = len;
  f->pool = f->hp = pool;
  f->loop = f->sp = pool + len;
  f->dict = nil;
  f->t0 = clock();
  return Ok; }

word dict_assoc(state f, word k) {
  return assoc(f, f->dict, k); }
