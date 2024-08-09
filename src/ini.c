#include "i.h"
#define P(n,...) { n, ((union cell[]){__VA_ARGS__})}
#define P2(n,...) { n, ((union cell[]){{cur}, {.x=putnum(2)},__VA_ARGS__})}
#define P3(n,...) { n, ((union cell[]){{cur}, {.x=putnum(3)},__VA_ARGS__})}

static struct { const char *n; union cell *x; } ini_dict[] = {
  P2("+",  {add }), P2("-",  {sub}),
  P2("*",  {mul }), P2("/",  {quot}),
  P2("%",  {rem}), P2("=",  {eq }),
  P2("<",  {lt}), P2("<=",  {le}),
  P2(">=",  {ge}), P2(">",  {gt}),
  P(".", {print}),
  P("putc",  {prc}),
  P("~",  {not}),
  P2("X",  {xons}),
  P("A",  {car}), P("B",  {cdr}),
  P2("sget",  {sget}), P3("ssub",  {ssub}), P("slen",  {slen}),
  P("s?",  {Sp}), P("n?", {Np}), P("X?",  {Xp}),
  P2("::", {mbind}),
  P("peek", {peek}),
  P2("poke", {poke}),
  P("trim",  {trim}), P2("seek",  {seek}),
  P("thd", {thda}), };

status l_ini(core f) {
  memset(f, 0, sizeof(struct core));
  const size_t len = 1;
  word *pool = l_malloc(2 * len * sizeof(word));
  if (!pool) return Oom;
  f->rand = f->t0 = clock();
  f->hp = f->pool = pool;
  f->loop = f->sp = pool + (f->len = len);
  f->dict = f->macro = nil;
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++) {
    string s = strof(f, ini_dict[i].n);
    pair w = s ? cons(f, (word) s, (word) ini_dict[i].x) : 0,
         x = w ? cons(f, (word) w, f->dict) : 0;
    if (!(f->dict = (word) x)) return l_fin(f), Oom; }
  return Ok; }

void l_fin(state f) {
  l_free(f->pool < f->loop ? f->pool : f->loop);
  f->pool = f->loop = NULL; }
