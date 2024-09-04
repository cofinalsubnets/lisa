#include "i.h"
#define P(n,i) { n, ((union cell[]){{i}})}
#define P2(n,i) { n, ((union cell[]){{cur}, {.x=putnum(2)},{i}})}
#define P3(n,i) { n, ((union cell[]){{cur}, {.x=putnum(3)},{i}})}

struct function_entry {
  const char *nom;
  union cell *val;
} ini_dict[] = {
  P2("+",  add), P2("-",  sub),
  P2("*",  mul), P2("/",  quot),
  P2("%",  rem), P2("=",  eq),
  P2("<",  lt), P2("<=",  le),
  P2(">=",  ge), P2(">",  gt),
  P(".", print), P("putc",  prc),
  P("~",  not),
  P2("X",  cons), P("A",  car), P("B",  cdr),
  P2("sget",  sget), P3("ssub",  ssub), P("slen",  slen),
  P("s?",  Sp), P("n?", Np), P("X?",  Xp),
  P2("::", mbind),
  P("peek", peek),
  P2("poke", poke),
  P("trim",  trim), P2("seek",  seek),
  P("tnew", tnew), P("tkeys", tkeys), P("tlen", tlen),
  P3("tset", tset), P3("tget", tget), P3("tdel", tdel),
  P("gensym", gensym),
  P("thd", thda), };

status initialize(core f, size_t len, word *pool, word *loop) {
  memset(f, 0, sizeof(struct l_core));
  f->pool = pool, f->loop = loop;
  f->rand = f->t0 = clock();
  f->len = len, f->pool = pool, f->loop = loop;
  f->hp = pool, f->sp = pool + len;
  if (!(f->dict = new_table(f))) return Oom;
  if (!(f->macro = new_table(f))) return Oom;
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++) {
    word k = (word) literal_string(f, ini_dict[i].nom),
         v = (word) ini_dict[i].val;
    if (!k || !table_set(f, f->dict, k, v)) return Oom; }
  return Ok; }
