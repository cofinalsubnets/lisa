#include "lips.h"
#include "terp.h"
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <errno.h>

#define NOM "lips"
#define BOOT "prelude."NOM
#define USR_PATH ".local/lib/"NOM"/"
#define SYS_PATH "/usr/lib/"NOM"/"
static Inline int seekp(const char* p) {
 int b, c;
 b = openat(AT_FDCWD, getenv("HOME"), O_RDONLY);
 c = openat(b, USR_PATH, O_RDONLY), close(b);
 b = openat(c, p, O_RDONLY), close(c);
 if (b > -1) return b;
 b = openat(AT_FDCWD, SYS_PATH, O_RDONLY);
 c = openat(b, p, O_RDONLY), close(b);
 return c; }

obj script(lips v, const char *path, FILE *f) {
 if (!f) return
   errp(v, "%s : %s", path, strerror(errno)),
   lips_fin(v),
   NO;
 jmp_buf re;
 v->restart = &re;
 if (setjmp(re)) return
  errp(v, "%s : fail", path),
  fclose(f), (obj) lips_fin(v);
 obj x;
 while ((x = parse(v, f))) eval(v, x);
 return x = feof(f) ? x : 0, fclose(f), x; }

int lips_boot(lips v) {
 const char * const path = BOOT;
 int pre = seekp(path);
 if (pre == -1) return errp(v, "can't find %s", path), NO;
 return xval(script(v, path, fdopen(pre, "r"))); }

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
 with(y,
  spush(v, table(v)),
#define repr(a,b)defprim(v,a,b)
#define rein(a)rin(v, "i-"#a,a)
  prims(repr), insts(rein),
  Top = spop(v),
  z = table(v), Mac = z,
#define bsym(i,s)(z=interns(v,s),AR(y)[i]=z)
  bsym(Eval, "ev"), bsym(Apply, "ap"),
  bsym(Def, ":"),   bsym(Cond, "?"), bsym(Lamb, "\\"),
  bsym(Quote, "`"), bsym(Seq, ","),  bsym(Splat, "."));
#define def(s, x) (y=interns(v,s),tblset(v,Top,y,x))
 def("ns", Top), def("macros", Mac);
 return v; }
