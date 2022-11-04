#include "la.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>

// initialization helpers
static bool defprims(la);
static ob
  inst(la, const char*, vm*) NoInline,
  symofs(la, const char*) NoInline;

ob la_ev(la v, ob _) {
  static struct mo go[] = { {call}, {(vm*) putnum(1)}, {yield} };
  return !pushs(v, _, NULL) ? 0 :
    call(v, (ob) prims, go, v->hp, v->sp, v->fp); }

NoInline bool la_script(la v, const char *path) {
  FILE *in = fopen(path, "r");
  if (!in) return
    errp(v, "%s : %s", path, strerror(errno)),
    false;
  bool ok = true;
  for (ob x; ok && !feof(in);
    x = la_rx(v, in),
    ok = x ? la_ev(v, x) : feof(in));
  if (!ok) errp(v, "%s : %s", path, "error");
  return fclose(in), ok; }

la la_ini(void) {
  la v = malloc(sizeof(struct la));
  if (v && !la_open(v)) return la_fin(v), NULL;
  return v; }

void la_fin(la v) { la_close(v), free(v); }

void la_close(la v) {
  if (v) free(v->pool), v->pool = NULL; }

NoInline bool la_open(la v) {
  v->rand = v->t0 = clock();
  v->len = 1 << 10;
  v->pool = NULL;
  v->safe = NULL;
  // the heap is all used up to start, so the first allocation initializes the pool
  v->hp = v->sp = v->pool + v->len;
  v->fp = (sf) v->sp;
  v->topl = (tbl) nil;
  v->ip = (mo) nil;
  v->xp = nil;
  v->syms = 0;
  setw(v->lex, nil, LexN);

  ob _;
  bool ok =
    // global symbols // FIXME stop using these if possible
    (v->lex[Eval] = symofs(v, "ev")) &&
    (v->lex[Def] = symofs(v, ":")) &&
    (v->lex[Cond] = symofs(v, "?")) &&
    (v->lex[Lamb] = symofs(v, "\\")) &&
    (v->lex[Quote] = symofs(v, "`")) &&
    (v->lex[Seq] = symofs(v, ",")) &&
    (v->lex[Splat] = symofs(v, ".")) &&

    // make the global namespace
    (v->topl = mktbl(v)) &&
    (_ = symofs(v, "_ns")) &&
    tblset(v, v->topl, _, (ob) v->topl)
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define reg_intl(a) && inst(v, "i-"#a, a)
    i_internals(reg_intl)
    && defprims(v);

  if (!ok) la_close(v);
  return ok; }

static NoInline ob symofs(la v, const char *s) {
  str _ = strof(v, s);
  return _ ? (ob) symof(v, _) : 0; }

// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
const struct prim prims[] = { i_primitives(prim_ent) };

#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(mo x) {
  struct prim *_ = (struct prim*) x;
  return _ >= prims && _ < prims + LEN(prims); }

static bool defprims(la v) {
  const struct prim *p = prims,
                    *lim = p + LEN(prims);
  while (p < lim) {
    ob z = symofs(v, p->nom);
    if (!z || !tblset(v, v->topl, z, (ob) p++)) return false; }
  return true; }

// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline ob inst(la v, const char *a, vm *b) {
  ob z = symofs(v, a);
  return z ? tblset(v, v->topl, z, putnum(b)) : 0; }

