#include "lisa.h"
#include "vm.h"
#include <time.h>
#include <stdlib.h>

// static table of primitive functions
#define prim_ent(go, nom) { go, nom },
struct prim primitives[] = { i_primitives(prim_ent) };

#define LEN(ary) (sizeof(ary)/sizeof(*ary))
bool primp(ob x) {
  struct prim *_ = (struct prim*) x;
  return _ >= primitives && _ < primitives + LEN(primitives); }

static bool define_primitives(la v) {
  struct prim *p = primitives,
              *lim = p + LEN(primitives);
  for (;p < lim; p++) {
    ob z = interns(v, p->nom);
    if (!z || !tbl_set(v, v->topl, z, (ob) p)) return false; }
  return true; }
// initialization helpers
//
// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline ob inst(la v, const char *a, vm *b) {
  ob z = interns(v, a);
  return z ? tbl_set(v, v->topl, z, putnum(b)) : 0; }

// initialize a process
static bool la_ini_(la v) {
  // set time & random seed
  v->rand = v->t0 = clock();

  // configure memory
  // how big a memory pool to start with?
  v->len = 1 << 10;
  // there is no pool yet
  v->pool = NULL;
  // no protected values
  v->keep = NULL;
  // the data stack starts at the top of memory
  // the call stack lives on the data stack
  // the heap is all used up to start, so the first
  // allocation initializes the pool
  v->fp = (fr) (v->hp = v->sp = v->pool + v->len);
  // everything else starts empty
  v->ip = (mo) (v->topl = v->syms = v->xp = nil);
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
    && define_primitives(v);

  if (!ok) la_fin(v);
  return ok; }

la la_ini(void) {
  la v = malloc(sizeof(struct la));
  if (v && !la_ini_(v)) v = NULL;
  return v; }

void la_fin(la v) { if (v) free(v->pool), free(v); }
