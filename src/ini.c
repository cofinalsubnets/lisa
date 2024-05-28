#include "i.h"
void l_fin(state f) {
  f->free(f->pool < f->loop ? f->pool : f->loop);
  f->pool = f->loop = NULL; }

#define binop() {cur}, {.x = putnum(2)}
static union cell
  p_print[] = {{print}},
  p_not[] = {{not}},
  p_cons[] = {binop(), {xons}},
  p_car[] = {{car}},
  p_cdr[] = {{cdr}},
  p_le[] = {binop(), {le}},
  p_lt[] = {binop(), {lt}},
  p_eq[] = {binop(), {eq}},
  p_ge[] = {binop(), {ge}},
  p_gt[] = {binop(), {gt}},
  p_slen[] = {{slen}},
  p_sget[] = {binop(), {sget}},
  p_ssub[] = {{cur}, {.x = putnum(3)}, {ssub}},
  p_2p[] = {{Xp}},
  p_np[] = {{Np}},
  p_sp[] = {{Sp}},
  p_mbind[] = {binop(), {mbind}},
  p_seek[] = {binop(), {seek}},
  p_peek[] = {{peek}},
  p_poke[] = {binop(), {poke}},
  p_trim[] = {{trim}},
  p_thd[] = {{thda}},
//  p_p[] = { {pr}},
//  p_pp[] = { {ppr} },
//  p_sp[] = { {spr} },
//  p_psp[] = { {pspr} },
  p_pc[] = {{prc}},
  p_quot[] = {binop(), {quot}},
  p_rem[] = {binop(), {rem}},
  p_mul[] = {binop(), {mul}},
  p_sub[] = {binop(), {sub}},
  p_add[] = {binop(), {add}};

status l_ini(state f, const size_t len, malloc_t *_malloc, free_t *_free) {
  memset(f, 0, sizeof(struct core));
  word *pool = _malloc(2 * len * sizeof(word));
  if (!pool) return Oom;
  f->rand = f->t0 = clock();
  f->hp = f->pool = pool;
  f->malloc = _malloc, f->free = _free;
  f->loop = f->sp = pool + (f->len = len);
  f->dict = f->macro = nil;
  static struct { const char *n; word x; } ini_dict[] = {
    { "+", (word) p_add }, { "-", (word) p_sub },
    { "*", (word) p_mul }, { "/", (word) p_quot },
    { "%", (word) p_rem }, { "=", (word) p_eq },
    { "<", (word) p_lt }, { "<=", (word) p_le },
    { ">=", (word) p_ge }, { ">", (word) p_gt },
    { ".", (word) p_print },
    { "putc", (word) p_pc },
    { "~", (word) p_not },
    { "X", (word) p_cons }, { "A", (word) p_car }, { "B", (word) p_cdr },
    { "sget", (word) p_sget}, { "ssub", (word) p_ssub}, { "slen", (word) p_slen},
    { "s?", (word) p_sp}, { "n?", (word) p_np}, { "X?", (word) p_2p},
    { "::", (word) p_mbind}, // defmacro
    { "peek", (word) p_peek}, { "poke", (word) p_poke},
    { "trim", (word) p_trim}, { "seek", (word) p_seek},
    { "thd", (word) p_thd},
  };
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++) {
    string s = strof(f, ini_dict[i].n);
    pair w = s ? cons(f, (word) s, ini_dict[i].x) : 0,
         x = w ? cons(f, (word) w, f->dict) : 0;
    if (!(f->dict = (word) x)) return l_fin(f), Oom; }
  return Ok; }
