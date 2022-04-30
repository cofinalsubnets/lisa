#include "lips.h"
#include "vm.h"
#include <time.h>

// initialization helpers
SNI u1 inst(en v, const char *a, vm *b) {
  ob z;
  bind(z, interns(v, a));
  return !!tbl_set(v, Top, z, putnum((i64) b)); }

SNI u1 prim(en v, const char *a, vm *i) {
  ob nom;
  yo prim;
  bind(nom, pair(v, interns(v, a), nil));
  with(nom, prim = cells(v, 4));
  bind(prim, prim);
  prim[0].ll = i;
  prim[1].ll = (vm*) nom;
  prim[2].ll = NULL;
  prim[3].ll = (vm*) prim;
  return !!tbl_set(v, Top, A(nom), (ob) prim); }

// lips destructor
u0 li_fin(en v) { if (v) free(v->pool), free(v); }

// lips constructor
en li_ini(void) {
  en v; ob _;
  bind(v, malloc(sizeof(struct en)));

  u64 t0 = clock();
  v->t0 = t0;
  v->rand = lcprng(t0 * mix);
  v->len = 1;
  v->pool = NULL;
  v->mm = NULL;
  v->fp = (fr) (v->hp = v->sp = (void*) sizeof (void*));
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
