#include "lips.h"
#include "terp.h"
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <errno.h>

#ifndef PREFIX
#define PREFIX "/usr/local"
#endif

obj script(lips v, const char *path, FILE *f) {
 if (!f) return
   errp(v, "%s : %s", path, strerror(errno)),
   lips_fin(v),
   0;
 jmp_buf re;
 v->restart = &re;
 if (setjmp(re)) return
  errp(v, "%s : fail", path),
  fclose(f),
  (obj) lips_fin(v);
 obj x;
 while ((x = parse(v, f))) eval(v, x);
 return x = feof(f) ? (x || nil) : 0, fclose(f), x; }

int lips_boot(lips v) {
 static const char *path = PREFIX "/lib/lips/prelude.lips";
 return xval(script(v, path, fopen(path, "r"))); }

lips lips_open() {
 lips v = malloc(sizeof(struct lips));
 if (v)
  if (!lips_init(v) || lips_boot(v) != OK)
   return lips_close(v);
 return v; }

lips lips_close(lips v) { return
 lips_fin(v), free(v), NULL; }

lips lips_fin(lips v) { return
 free(v->mem_pool), (lips) (v->mem_pool = NULL); }

static NoInline u0 rin(lips v, const char *a, terp *b) {
 obj z = interns(v, a);
 tblset(v, *Sp, z, Pn(b)); }

obj lips_eval(lips v, char *expr) { return
  script(v, "eval", fmemopen(expr, slen(expr), "r")); }

lips lips_init(lips v) {
 v->seed = v->t0 = clock(),
 v->ip = v->xp = v->syms = v->glob = nil,
 v->fp = v->hp = v->sp = (mem) W,
 v->count = 0, v->mem_len = 1, v->mem_pool = NULL,
 v->mem_root = NULL;
 jmp_buf re;
 v->restart = &re;
 if (setjmp(re)) return lips_fin(v);
 vec t = cells(v, Size(tup) + NGlobs);
 set64(t->xs, nil, t->len = NGlobs);
 obj z, y = Glob = puttup(t);
 mm(&y);
  spush(v, table(v));
#define repr(a, b) if (b) defprim(v,b,a);
#define rein(a, b) if (!b) rin(v, "i-"#a,a);
  insts(repr) insts(rein)
  Top = spop(v),
  z = table(v), Mac = z,
#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
  bsym(Eval, "ev"), bsym(Apply, "ap"),
  bsym(Def, ":"),   bsym(Cond, "?"), bsym(Lamb, "\\"),
  bsym(Quote, "`"), bsym(Seq, ","),  bsym(Splat, ".");
#define def(s, x) (y=interns(v,s),tblset(v,Top,y,x))
 def("ns", Top), def("macros", Mac);
 return um, v; }
