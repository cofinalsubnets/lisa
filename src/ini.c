#include "i.h"
#define cur2 {cur}, {.x = putnum(2)}
static union cell
  p_print[] = {{print}},
  p_not[] = {{not}},
  p_cons[] = {cur2, {xons}},
  p_car[] = {{car}},
  p_cdr[] = {{cdr}},
  p_le[] = {cur2, {le}},
  p_lt[] = {cur2, {lt}},
  p_eq[] = {cur2, {eq}},
  p_ge[] = {cur2, {ge}},
  p_gt[] = {cur2, {gt}},
  p_slen[] = {{slen}},
  p_sget[] = {cur2, {sget}},
  p_ssub[] = {{cur}, {.x = putnum(3)}, {ssub}},
  p_2p[] = {{Xp}},
  p_np[] = {{Np}},
  p_sp[] = {{Sp}},
  p_mbind[] = {cur2, {mbind}},
  p_seek[] = {cur2, {seek}},
  p_peek[] = {{peek}},
  p_poke[] = {cur2, {poke}},
  p_trim[] = {{trim}},
  p_thd[] = {{thda}},
//  p_p[] = { {pr}},
//  p_pp[] = { {ppr} },
//  p_sp[] = { {spr} },
//  p_psp[] = { {pspr} },
  p_pc[] = {{prc}},
  p_quot[] = {cur2, {quot}},
  p_rem[] = {cur2, {rem}},
  p_mul[] = {cur2, {mul}},
  p_sub[] = {cur2, {sub}},
  p_add[] = {cur2, {add}};
#undef cur2

static struct { const char *n; union cell *x; } ini_dict[] = {
  { "+",  p_add }, { "-",  p_sub },
  { "*",  p_mul }, { "/",  p_quot },
  { "%",  p_rem }, { "=",  p_eq },
  { "<",  p_lt }, { "<=",  p_le },
  { ">=",  p_ge }, { ">",  p_gt },
  { ".",  p_print },
  { "putc",  p_pc },
  { "~",  p_not },
  { "X",  p_cons }, { "A",  p_car }, { "B",  p_cdr },
  { "sget",  p_sget}, { "ssub",  p_ssub}, { "slen",  p_slen},
  { "s?",  p_sp}, { "n?",  p_np}, { "X?",  p_2p},
  { "::",  p_mbind}, // defmacro
  { "peek",  p_peek}, { "poke",  p_poke},
  { "trim",  p_trim}, { "seek",  p_seek},
  { "thd",  p_thd}, };

status l_ini(state f, const size_t len) {
  memset(f, 0, sizeof(struct core));
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
