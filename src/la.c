#include "la.h"
#include "str.h"
#include "sym.h"
#include "tbl.h"
#include "alloc.h"
#include "vm.h"
#include "mo.h"
#include "gc.h"
#include <string.h>

// initialization helpers
static bool
  defprims(la),
  inst(la, const char*, vm*);
static sym symofs(la, const char*);

void la_reset(la_carrier v) {
  v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->ip = 0;
  v->xp = nil; }

void la_close(la v) {
  if (v) free(v->pool), v->pool = NULL; }

static sym *la_open_lexicon(la_carrier v) {
  sym *lex = (sym*) mkmo(v, LexN);
  if (!lex) return NULL;
  setw(lex, nil, LexN);
  mm(&lex);
  if (!(lex[Eval] = symofs(v, "ev")) ||
      !(lex[Def] = symofs(v, ":")) ||
      !(lex[Cond] = symofs(v, "?")) ||
      !(lex[Lamb] = symofs(v, "\\")) ||
      !(lex[Quote] = symofs(v, "`")) ||
      !(lex[Seq] = symofs(v, ",")) ||
      !(lex[Splat] = symofs(v, ".")))
    lex = NULL;
  return um, lex; }

// FIXME return a tbl
static bool la_open_toplevel(la_carrier v) {
  ob _; return
    (v->topl = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tbl_set(v, v->topl, _, (ob) v->topl)
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define reg_intl(a) && inst(v, "i-"#a, a)
    i_internals(reg_intl)
    && defprims(v); }

la_status la_open(la_carrier v) {
  const size_t len = 1 << 10; // must be a power of 2

  ob *pool = calloc(len, sizeof(ob));
  if (!pool) return LA_XOOM;

  memset(v, 0, sizeof(struct la_carrier));

  v->len = len;
  v->hp = v->pool = pool;
  v->hp = pool;
  v->sp = pool + len;
  v->fp = (sf) v->sp;

  v->rand = v->run.t0 = clock();

  bool ok =
    (v->lex = la_open_lexicon(v)) &&
    la_open_toplevel(v);

  if (!ok) la_close(v);
  return ok ? LA_OK : LA_XOOM; }

static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
const struct la_prim prims[] = { i_primitives(prim_ent) };
#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(mo x) {
  return x >= (mo) prims && x < (mo) (prims + LEN(prims)); }

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
