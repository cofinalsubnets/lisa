#include "la.h"
#include <string.h>

static sym symofs(la, const char*);
static str strof(la, const char*);
static u1
  defprim(la, vm*, const char*) NoInline,
  inst(la, const char*, vm*),
  la_ini_(la);

enum status la_ini(la_carrier v) {
  const U len = 1 << 10; // power of 2
  memset(v, 0, sizeof(struct carrier));

  ob *pool = malloc(len * sizeof(ob));
  if (!pool) return LA_XOOM;

  v->len = len;
  v->hp = v->pool = pool;
  v->fp = (sf) (v->sp = pool + len);
  v->rand = v->run.t0 = clock();
  return la_ini_(v) ? LA_OK : (la_fin(v),  LA_XOOM); }

u0 la_fin(struct carrier *v) {
  if (v) free(v->pool), v->pool = NULL; }

u0 la_reset(struct carrier *v) {
  v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->ip = 0;
  v->xp = nil; }

static str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, wsizeof(struct str) + b2w(bs));
  if (o) memcpy(o->text, c, bs), str_ini(o, bs);
  return o; }

// initialization helpers
static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

#define dp(go, nom) && defprim(v, go, nom)
static NoInline u1 defprim(la v, vm *i, const char *n) {
  sym y = symofs(v, n);
  if (!y) return false;
  mo k; with(y, k = mo_n(v, 2));
  if (!k) return false;
  return k[0].ap = i,
         k[1].ap = (vm*) y,
         tbl_set(v, v->topl, (ob) y, (ob) k); }

#define reg_intl(a) && inst(v, "i-"#a, a)
static u1 la_ini_(la v) {
  sym y; ob _; return
    (y = symofs(v, "ev"), v->lex.eval = y) &&
    (y = symofs(v, ":"), v->lex.define = y) &&
    (y = symofs(v, "?"), v->lex.cond = y) &&
    (y = symofs(v, "\\"), v->lex.lambda = y) &&
    (y = symofs(v, "`"), v->lex.quote = y) &&
    (y = symofs(v, ","), v->lex.begin = y) &&
    (y = symofs(v, "."), v->lex.splat = y) &&
    (v->topl = mktbl(v)) i_internals(reg_intl) &&
    (v->macros = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->topl, _, (ob) v->topl) &&
    (_ = (ob) symofs(v, "macros")) &&
    tbl_set(v, v->topl, _, (ob) v->macros)
    i_primitives(dp); }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static u1 inst(la v, const char *a, vm *b) {
  sym z = symofs(v, a);
  return z && tbl_set(v, v->topl, (ob) z, (ob) b); }

