#include "i.h"

void l_fin(state f) { if (f)
  free(f->pool < f->loop ? f->pool : f->loop),
  f->pool = f->loop = NULL; }

#define binop() {curry}, {.x = putnum(2)}
static union cell
  p_print[] = { {print} },
  p_eql[] = { binop(), {eqp}, },
  p_not[] = { {not} },
  p_lt[] = { binop(), {lt}, },
  p_le[] = { binop(), {le} },
  p_gt[] = { binop(), {gt}, },
  p_ge[] = { binop(), {ge} },
  p_add[] = { binop(), {add}, };
static status l_ini_dict(state f) {
  static struct { const char *n; word x; } ini_dict[] = {
    { "+", (word) p_add },
    { "=", (word) p_eql },
    { "<", (word) p_lt },
    { "<=", (word) p_le },
    { ">", (word) p_gt },
    { ">=", (word) p_ge },
    { ".", (word) p_print },
  };
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++) {
    string s = strof(f, ini_dict[i].n);
    pair w = s ? cons(f, (word) s, ini_dict[i].x) : 0,
         x = w ? cons(f, (word) w, f->dict) : 0;
    if (!x) return Oom;
    f->dict = (word) x; }
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
