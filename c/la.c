#include "la.h"
#include <time.h>
#include <stdlib.h>

// finalize a process
void la1(ps v) {
  if (!v) return;
  for (ob f = v->fins; twop(f); f = B(f))
    ((finalizer*) getZ(BA(f)))(v, AA(f));
  free(v->pool);
  free(v); }

// initialization helpers
//
// store an instruction address under a variable in the
// toplevel namespace // FIXME use a different namespace
static NoInline bool inst(em v, const char *a, ll *b) {
  ob z; return !(z = interns(v, a)) ? 0 :
    !!tbl_set(v, A(v->wns), z, putnum(b)); }

// make a primitive function
static NoInline bool prim(em v, const char *a, ll *i) {
  ob nom; mo k; return
    (nom = interns(v, a)) &&
    (nom = pair(v, nom, nil)) &&
    (with(nom, k = cells(v, 4)), k) ?
      !!tbl_set(v, A(v->wns), A(nom), (ob)
        (k[0].ll = i,    k[1].ll = (ll*) nom,
         k[2].ll = NULL, k[3].ll = (ll*) k)) : 0; }


// initialize a process
la la0(void) {
  ob _; em v = malloc(sizeof(struct ps));
  return v && (
     // set time & random seed
     v->t0 = clock(),
     v->rand = v->t0,

     // configure memory
     // how big a memory pool do we start with?
#define InitialPoolSize (1<<10)
     v->len = InitialPoolSize,
     // obviously there's no pool yet
     v->pool = NULL,
     // nor any protected values
     v->keep = NULL,
     // the data stack starts at the top of memory
     v->sp = v->pool + v->len,
     // the call stack lives on the data stack
     v->fp = (fr) v->sp,
     // the heap is all used up to start, so the first
     // allocation initializes the pool
     v->hp = v->sp,
     // everything else starts empty
     v->ip = (mo) nil,
     v->fins = v->wns = v->sns = v->syms = v->xp = nil,
     setw(v->lex, nil, LexN),

     // now we can start allocating.
     // global symbols // FIXME stop using these if possible
     (v->lex[Eval] = interns(v, "ev")) &&
     (v->lex[Apply] = interns(v, "ap")) &&
     (v->lex[Def] = interns(v, ":")) &&
     (v->lex[Cond] = interns(v, "?")) &&
     (v->lex[Lamb] = interns(v, "\\")) &&
     (v->lex[Quote] = interns(v, "`")) &&
     (v->lex[Seq] = interns(v, ",")) &&
     (v->lex[Splat] = interns(v, ".")) &&

     // make the toplevel namespace and initialize the cwd
     (_ = table(v)) &&
     (v->wns = pair(v, _, nil)) &&

     // create the directory and store toplevel at 0
     (v->sns = table(v)) &&
     (tbl_set(v, v->sns, nil, A(v->wns))) // &&
     // register instruction addresses at toplevel so the
     // compiler can use them.
#define register_inst(a, b) && ((b) ? prim(v,b,a) : inst(v, "i-"#a,a))
     insts(register_inst))
    ? v : (la1(v), NULL); }
