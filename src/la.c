#include "la.h"
#include <string.h>

static str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, wsizeof(struct str) + b2w(bs));
  if (o) memcpy(o->text, c, bs), ini_str(o, bs);
  return o; }

// initialization helpers
static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

static bool defprims(la), inst(la, const char*, vm*);

void la_reset(la_carrier v) {
  v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->ip = 0;
  v->xp = nil; }

void la_close(la v) { if (v) la_free(v->pool), v->pool = NULL; }

static bool la_open_lexi(la_carrier v) { return
  (v->lex.eval = symofs(v, "ev")) &&
  (v->lex.define = symofs(v, ":")) &&
  (v->lex.cond = symofs(v, "?")) &&
  (v->lex.lambda = symofs(v, "\\")) &&
  (v->lex.quote = symofs(v, "`")) &&
  (v->lex.begin = symofs(v, ",")) &&
  (v->lex.splat = symofs(v, ".")); }

static bool la_open_topl_boot(la v) {
  ob _; return
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->topl, _, (ob) v->topl) &&
    (_ = (ob) symofs(v, "macros")) &&
    tbl_set(v, v->topl, _, (ob) v->macros) &&
    defprims(v); }

#define reg_intl(a) && inst(v, "i-"#a, a)
static bool la_open_topl(la_carrier v) { return
  (v->topl = mktbl(v)) i_internals(reg_intl) &&
  (v->macros = mktbl(v)) &&
  la_open_topl_boot(v); }

enum la_status la_open(la_carrier v) {
  memset(v, 0, sizeof(struct la_carrier));
  const size_t len = 1 << 10; // must be a power of 2
  ob *pool = la_calloc(len, sizeof(ob));
  return !pool ? LA_XOOM : (
    v->len = len,
    v->hp = v->pool = pool,
    v->fp = (sf) (v->sp = pool + len),
    v->rand = v->run.t0 = la_clock(),
    la_open_lexi(v) && la_open_topl(v) ?
      LA_OK : (la_close(v), LA_XOOM)); }

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
