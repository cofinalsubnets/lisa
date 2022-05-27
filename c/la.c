#include "la.h"
#include <stdlib.h>
#include <time.h>

// initialization helpers
static NoInline bool
  inst(la, const char *, go *),
  prim(la, const char *, go *);

ob ls(em v, ob _) {
  ob x, mod = v->wns;
  for (; twop(mod); mod = B(mod))
    if ((x = tbl_get(v, A(mod), _))) return x;
  return 0; }

// bootstrap eval interpreter function
Ll(ev_u) {
  Arity(1); mo y;
  return
    // check to see if ev has been overridden in the
    // toplevel namespace and if so call that. this way
    // ev calls compiled pre-bootstrap will use the
    // bootstrapped compiler, which is what we want?
    // seems kind of strange to need this ...
    xp = ls(v, v->lex[Eval]),
    xp && homp(xp) && gethom(xp)->ll != ev_u ?
      ApY((mo) xp, nil) :
      // otherwise use the bootstrap compiler.
      !(Pack(), y = ana(v, *fp->argv, putnum(ret))) ? 0 :
        (Unpack(), ApY(y, xp)); }
la la0(void) {
  ob _; em v = malloc(sizeof(struct ph));
  return v && (
     // set time & random seed
     v->t0 = clock(),
     v->rand = lcprng(v->t0),

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
     v->wns = v->sns = v->syms = v->xp = nil,
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

// finalize a vm. 
void la1(la v) { if (v) free(v->pool), free(v); }

static NoInline bool inst(em v, const char *a, ll *b) {
  ob z; return !(z = interns(v, a)) ? 0 :
    !!tbl_set(v, A(v->wns), z, putnum(b)); }

static NoInline bool prim(em v, const char *a, ll *i) {
  ob nom; mo k; return
    (nom = interns(v, a)) &&
    (nom = pair(v, nom, nil)) &&
    (with(nom, k = cells(v, 4)), k) ?
      !!tbl_set(v, A(v->wns), A(nom), (ob)
        (k[0].ll = i,    k[1].ll = (ll*) nom,
         k[2].ll = NULL, k[3].ll = (ll*) k)) : 0; }
