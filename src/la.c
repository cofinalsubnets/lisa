#include "la.h"
#include "str.h"
#include "sym.h"
#include "tbl.h"
#include "alloc.h"
#include "vm.h"
#include "lexicon.h"
#include <string.h>

// return to C
static Vm(yield) { return Pack(), LA_OK; }

static ob la_ev(la v, ob _) {
  if (!pushs(v, _, NULL)) return 0;
  ob ev = tblget(v, v->topl, (ob) v->lex[Eval]);
  struct mo go[] = { {call}, {(vm*) putnum(1)}, {yield} };
  return call(v, ev, go, v->hp, v->sp, v->fp); }

enum la_status la_ev_f(la_carrier v, FILE *in) {
  enum la_status s = la_rx_f(v, in);
  return s == LA_OK ? la_ev(v, v->xp) : s; }

// initialization helpers
static bool
  defprims(la),
  inst(la, const char*, vm*);
static sym symofs(la, const char*);

void la_reset(la v) {
  v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->ip = 0;
  v->xp = 0; }

void la_close(la v) {
  if (v) free(v->pool), v->pool = NULL; }

la_status la_open(la v) {
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

  ob _;
  bool ok =
    // global symbols // FIXME stop using these if possible
    (v->lex[Eval] = symofs(v, LA_LEX_EVAL)) &&
    (v->lex[Def] = symofs(v, LA_LEX_DEFINE)) &&
    (v->lex[Cond] = symofs(v, LA_LEX_COND)) &&
    (v->lex[Lamb] = symofs(v, LA_LEX_LAMBDA)) &&
    (v->lex[Quote] = symofs(v, LA_LEX_QUOTE)) &&
    (v->lex[Seq] = symofs(v, LA_LEX_BEGIN)) &&
    (v->lex[Splat] = symofs(v, LA_LEX_VARARG)) &&

    // make the global namespace
    (v->topl = mktbl(v)) &&
    (_ = (ob) symofs(v, "_ns")) &&
    tblset(v, v->topl, _, (ob) v->topl)
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define reg_intl(a) && inst(v, "i-"#a, a)
    i_internals(reg_intl)
    && defprims(v);

  if (!ok) la_close(v);
  return ok ? LA_OK : LA_XOOM; }

static sym symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? symof(v, _) : 0; }

// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
const struct prim prims[] = { i_primitives(prim_ent) };
#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(mo x) {
  return x >= (mo) prims && x < (mo) (prims + LEN(prims)); }

static bool defprims(la v) {
  const struct prim *p = prims, *lim = p + LEN(prims);
  while (p < lim) {
    sym z = symofs(v, p->nom);
    if (!z || !tblset(v, v->topl, (ob) z, (ob) p++)) return false; }
  return true; }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static bool inst(la v, const char *a, vm *b) {
  sym z = symofs(v, a);
  return z && tblset(v, v->topl, (ob) z, (ob) b); }
