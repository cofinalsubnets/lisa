#include "la.h"
#include <string.h>

static sym symofs(la, const char*);
static str strof(la, const char*);
static bool
  defprims(la),
  inst(la, const char*, vm*),
  la_ini_(la);

enum status la_ini(la_carrier v) {
  const size_t len = 1 << 10; // power of 2
  memset(v, 0, sizeof(struct carrier));

  ob *pool = malloc(len * sizeof(ob));
  if (!pool) return LA_XOOM;

  v->len = len;
  v->hp = v->pool = pool;
  v->fp = (sf) (v->sp = pool + len);
  v->rand = v->run.t0 = clock();
  return la_ini_(v) ? LA_OK : (la_fin(v),  LA_XOOM); }

void la_fin(struct carrier *v) {
  if (v) free(v->pool), v->pool = NULL; }

void la_reset(struct carrier *v) {
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

#define reg_intl(a) && inst(v, "i-"#a, a)
static bool la_ini_(la v) {
  ob _; return
    (v->lex.eval = symofs(v, "ev")) &&
    (v->lex.define = symofs(v, ":")) &&
    (v->lex.cond = symofs(v, "?")) &&
    (v->lex.lambda = symofs(v, "\\")) &&
    (v->lex.quote = symofs(v, "`")) &&
    (v->lex.begin = symofs(v, ",")) &&
    (v->lex.splat = symofs(v, ".")) &&
    (v->topl = mktbl(v)) i_internals(reg_intl) &&
    (v->macros = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->topl, _, (ob) v->topl) &&
    (_ = (ob) symofs(v, "macros")) &&
    tbl_set(v, v->topl, _, (ob) v->macros) &&
    defprims(v); }

static NoInline bool defprim(la v, vm *i, const char *n) {
  sym y = symofs(v, n);
  if (!y) return false;
  mo k; with(y, k = mkmo(v, 2));
  if (!k) return false;
  k[0].ap = i;
  k[1].ap = (vm*) y;
  return tbl_set(v, v->topl, (ob) y, (ob) k); }

static bool defprims(la v) {
#define dp(go, nom) && defprim(v, go, nom)
  return true i_primitives(dp); }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static bool inst(la v, const char *a, vm *b) {
  sym z = symofs(v, a);
  return z && tbl_set(v, v->topl, (ob) z, (ob) b); }
