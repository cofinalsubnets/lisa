#include "la.h"
#include <stdlib.h>

ob la_ev(la v, ob _) {
  if (!Push(_)) return 0;
  struct mo go[] = { {call}, {(vm*) putnum(1)}, {yield} };
  return call(v, (ob) prims, go, v->hp, v->sp, v->fp); }

void la_fin(la v) { if (v) free(v->pool), free(v); }
void la_atpanic(la v, ob (*p)(la)) { v->panic = p; }

static bool defprims(la);
static ob inst(la, const char*, vm*) NoInline;

la la_ini(void) {
  la v = malloc(sizeof(struct la));
  if (!v) return NULL;

  v->rand = v->t0 = clock();
  v->len = 1 << 10; // initial memory size
  v->pool = NULL;
  v->safe = NULL;
  v->panic = NULL;
  // the heap is all used up to start, so the first allocation initializes the pool
  v->fp = (fr) (v->hp = v->sp = v->pool + v->len);
  v->ip = (mo) (v->topl = v->xp = nil);
  v->syms = 0;
  setw(v->lex, nil, LexN);

  ob _;
  bool ok =
    // global symbols // FIXME stop using these if possible
    (v->lex[Eval] = interns(v, "ev")) &&
    (v->lex[Def] = interns(v, ":")) &&
    (v->lex[Cond] = interns(v, "?")) &&
    (v->lex[Lamb] = interns(v, "\\")) &&
    (v->lex[Quote] = interns(v, "`")) &&
    (v->lex[Seq] = interns(v, ",")) &&
    (v->lex[Splat] = interns(v, ".")) &&

    // make the global namespace
    (v->topl = table(v)) &&
    (_ = interns(v, "_ns")) &&
    tbl_set(v, v->topl, _, v->topl)
    // register instruction addresses at toplevel so the
    // compiler can use them.
#define reg_intl(a) && inst(v, "i-"#a, a)
    i_internals(reg_intl)
    && defprims(v);

  return ok ? v : (la_fin(v), NULL); }


// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
const struct prim prims[] = { i_primitives(prim_ent) };

#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(ob x) {
  struct prim *_ = (struct prim*) x;
  return _ >= prims && _ < prims + LEN(prims); }

static bool defprims(la v) {
  const struct prim *p = prims,
                    *lim = p + LEN(prims);
  while (p < lim) {
    ob z = interns(v, p->nom);
    if (!z || !tbl_set(v, v->topl, z, (ob) p++)) return false; }
  return true; }
// initialization helpers
//
// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline ob inst(la v, const char *a, vm *b) {
  ob z = interns(v, a);
  return z ? tbl_set(v, v->topl, z, putnum(b)) : 0; }
