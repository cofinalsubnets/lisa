#include "lips.h"
#include "tbl.h"
#include "terp.h"
#include "sym.h"
#include "mem.h"
#include "two.h"
#include "hom.h"
#include "num.h"
#include <time.h>

// initialization helpers
static u1 inst(lips, const char*, terp*) NoInline,
          prim(lips, const char*, terp*) NoInline;

// lips destructor
u0 li_fin(lips v) { if (v) free(v->pool), free(v); }

// lips constructor
lips li_ini(void) {
  lips v; obj _;
  bind(v, malloc(sizeof(struct lips)));

  u64 t0 = clock();
  v->t0 = t0;
  v->rand = lcprng(t0 * mix);
  v->count = 0;
  v->len = 1;
  v->pool = NULL;
  v->root = NULL;
  v->fp = v->hp = v->sp = (mem) word;
  v->ip = v->xp = v->syms = nil;
  set64(v->glob, nil, NGlobs);

#define Bind(x) if(!(x))goto fail
  Bind(Top = table(v));
  Bind(Mac = table(v));
#define register_inst(a, b)if(b){Bind(prim(v,b,a));}else{Bind(inst(v, "i-"#a,a));}
  insts(register_inst)
#define bsym(i,s) Bind(v->glob[i]=interns(v,s))
  bsym(Eval, "ev");
  bsym(Apply, "ap");
  bsym(Def, ":");
  bsym(Cond, "?");
  bsym(Lamb, "\\");
  bsym(Quote, "`");
  bsym(Seq, ",");
  bsym(Splat, ".");
#define def(s, x) Bind(_=interns(v,s));Bind(tbl_set(v,Top,_,x))
  def("_ns", Top);
  def("_macros", Mac);
  return v; fail:
  return li_fin(v), NULL; }

static NoInline u1 inst(lips v, const char *a, terp *b) {
  obj z;
  bind(z, interns(v, a));
  return !!tbl_set(v, Top, z, _N((i64) b)); }

static NoInline u1 prim(lips v, const char *a, terp *i) {
  obj nom; hom prim;
  bind(nom, pair(v, interns(v, a), nil));
  with(nom, prim = cells(v, 4));
  bind(prim, prim);
  prim[0] = i;
  prim[1] = (terp*) nom;
  prim[2] = NULL;
  prim[3] = (terp*) prim;
  return !!tbl_set(v, Top, A(nom), _H(prim)); }
