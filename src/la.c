#include "la.h"
#include <string.h>

static str strof(la v, const char* c) {
  size_t bs = strlen(c);
  str o = cells(v, wsizeof(struct str) + b2w(bs));
  if (o) memcpy(o->text, c, bs),
         ini_str(o, bs);
  return o; }

// initialization helpers
static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

static bool
  defprims(la),
  inst(la, const char*, vm*);

void la_reset(la_carrier v) {
  v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->ip = 0;
  v->xp = nil; }

void la_close(la v) {
  if (v) free(v->pool), v->pool = NULL; }

static bool la_open_lexicon(la_carrier v) {
  struct la_lexicon *g =
    (void*) mkmo(v, wsizeof(struct la_lexicon));
  if (!g) return false;
  memset(g, -1, sizeof(struct la_lexicon));
  v->lex = g;
  sym _;
  // split the assignment into two expressions
  // to ensure correct sequencing
  return (_ = symofs(v, "ev"), g->eval = _) &&
      (_ = symofs(v, ":"), g->define = _) &&
      (_ = symofs(v, "?"), g->cond = _) &&
      (_ = symofs(v, "\\"), g->lambda = _) &&
      (_ = symofs(v, "`"), g->quote = _) &&
      (_ = symofs(v, ","), g->begin = _) &&
      (_ = symofs(v, "."), g->splat = _); }

// FIXME return a tbl
#define reg_intl(a) inst(v, "i-"#a, a) &&
static bool la_open_toplevel(la_carrier v) {
  ob _; return
    (v->topl = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->topl, _, (ob) v->topl) &&
    i_internals(reg_intl) defprims(v); }

la_status la_open(la_carrier v) {
  memset(v, 0, sizeof(struct la_carrier));
  const size_t len = 1 << 10; // must be a power of 2
  ob *pool = calloc(len, sizeof(ob));
  if (!pool) return LA_XOOM;
  v->len = len;
  v->hp = v->pool = pool;
  v->fp = (sf) (v->sp = pool + len);
  v->rand = v->run.t0 = clock();
  return la_open_lexicon(v) && la_open_toplevel(v) ?
    LA_OK : (la_close(v), LA_XOOM); }

// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
const struct la_prim prims[] = { i_primitives(prim_ent) };
#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(mo x) {
  return x >= (mo) prims &&
    x < (mo) (prims + LEN(prims)); }

static bool defprims(la v) {
  const struct la_prim *p = prims, *lim = p + LEN(prims);
  while (p < lim) {
    sym z = symofs(v, p->nom);
    if (!z || !tbl_set(v, v->topl, (ob) z, (ob) p++)) return false; }
  return true; }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static bool inst(la v, const char *a, vm *b) {
  sym z = symofs(v, a);
  return z && tbl_set(v, v->topl, (ob) z, (ob) b); }
