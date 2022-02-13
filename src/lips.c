#include "lips.h"
#include "tbl.h"
#include "terp.h"
#include "sym.h"
#include "mem.h"
#include "two.h"
#include "hom.h"
#include <time.h>

// initialization helpers
static u0 inst(lips, const char*, terp*) NoInline,
          prim(lips, const char*, terp*) NoInline;

// lips destructor
u0 li_fin(lips v) { if (v) { if (v->pool) free(v->pool);
                             free(v); } }
// lips constructor
lips li_ini(void) {
  lips v;
  if (!(v = malloc(sizeof(struct lips)))) return v;
  v->t0 = clock();
  v->rand = LCPRNG(v->t0 * mix);
  v->count = 0;
  v->ip = v->xp = v->syms = nil;
  v->fp = v->hp = v->sp = (mem) W, v->len = 1;
  v->pool = (mem) (v->root = NULL);
  set64(v->glob, nil, NGlobs);

  jmp_buf re;
  v->restart = &re;
  if (setjmp(re)) return li_fin(v), NULL;

  Top = table(v);
  Mac = table(v);
#define repr(a, b) if (b) prim(v,b,a);
#define rein(a, b) if (!b) inst(v, "i-"#a,a);
  insts(repr)
  insts(rein)
#define bsym(i,s)(v->glob[i]=interns(v,s))
  bsym(Eval, "ev");
  bsym(Apply, "ap");
  bsym(Def, ":");
  bsym(Cond, "?");
  bsym(Lamb, "\\");
  bsym(Quote, "`");
  bsym(Seq, ",");
  bsym(Splat, ".");
  obj y;
#define def(s, x) (y=interns(v,s),tbl_set(v,Top,y,x))
  def("_ns", Top);
  def("_macros", Mac);
  v->restart = NULL;
  return v; }

static NoInline u0 inst(lips v, const char *a, terp *b) {
  obj z = interns(v, a);
  tbl_set(v, Top, z, _N((i64) b)); }

static NoInline u0 prim(lips v, const char *a, terp *i) {
  // FIXME fails via restart
  obj nom = pair(v, interns(v, a), nil);
  hom prim;
  with(nom, prim = cells(v, 4));
  prim[0] = i;
  prim[1] = (terp*) nom;
  prim[2] = NULL;
  prim[3] = (terp*) prim;
  tbl_set(v, Top, A(nom), _H(prim)); }
