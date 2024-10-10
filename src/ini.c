#include "i.h"
#define P1(n,i) { n, ((union cell[]){{i}})}
#define P2(n,i) { n, ((union cell[]){{curry}, {.x=putnum(2)},{i}})}
#define P3(n,i) { n, ((union cell[]){{curry}, {.x=putnum(3)},{i}})}

struct function_entry {
  const char *nom;
  union cell *val;
} ini_dict[] = {
  P2("+",  add), P2("-",  sub),
  P2("*",  mul), P2("/",  quot),
  P2("%",  rem), P2("=",  eq),
  P2("<",  lt), P2("<=",  le),
  P2(">=",  ge), P2(">",  gt),
  P1(".", print), P1("putc",  prc),
  P1("~",  not),
  P1("rand", rng),
  P2("X",  cons), P1("A",  car), P1("B",  cdr),
  P2("sget",  sget), P3("ssub",  ssub), P1("slen",  slen),
  P1("s?",  Sp), P1("n?", Np), P1("X?",  Xp),
  P2("::", defmacro),
  P1("peek", peek),
  P2("poke", poke),
  P1("trim",  trim), P2("seek",  seek),
  P1("tnew", tnew), P1("tkeys", tkeys), P1("tlen", tlen),
  P3("tset", tset), P3("tget", tget), P3("tdel", tdel),
  P1("gensym", gensym),
  P1("thd", thda), };

status l_ini(core f, bool (*please)(core, size_t), size_t len, word *pool) {
  word *loop = pool + len;
  memset(f, 0, sizeof(struct l_core));
  f->pool = pool, f->loop = loop;
  f->rand = f->t0 = clock();
  f->please = please;
  f->len = len, f->pool = pool, f->loop = loop;
  f->hp = pool, f->sp = pool + len;
  if (!(f->dict = new_table(f))) return Oom;
  if (!(f->macro = new_table(f))) return Oom;
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++) {
    word k = (word) literal_string(f, ini_dict[i].nom),
         v = (word) ini_dict[i].val;
    if (!k || !table_set(f, f->dict, k, v)) return Oom; }
  return Ok; }

void l_close(core f) {
  if (f) free(f->pool < f->loop ? f->pool : f->loop), free(f); }

l_core l_open(void) {
  core f = malloc(sizeof(struct l_core));
  if (!f) return NULL;
  const size_t len0 = 1;
  word *pool = malloc(2 * len0 * sizeof(word));
  if (!pool) return free(f), NULL;
  status s = l_ini(f, libc_please, len0, pool);
  return s == Ok ? f : (l_close(f), NULL); }
