#include "i.h"

static union cell
  p_add[] = { {curry}, {.x = putnum(2)}, {add}, };

void l_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

static status l_ini_dict(state f) {
  string s = strof(f, "+");
  pair w = s ? cons(f, (word) s, (word) p_add) : 0,
       x = w ? cons(f, (word) w, f->dict) : 0;
  if (!x) return Oom;
  f->dict = (word) x;
  return Ok; }

status l_ini(state f) {
  memset(f, 0, sizeof(struct lisa));
  const size_t len = 1;
  word *pool = malloc(2 * len * sizeof(word));
  if (!pool) return Oom;
  f->len = len;
  f->pool = f->hp = pool;
  f->loop = f->sp = pool + len;
  f->dict = nil;
  f->t0 = clock();
  status s = l_ini_dict(f);
  if (s != Ok) l_fin(f);
  return s; }
